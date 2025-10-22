(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Infix
open Lwt.Syntax

let catch_unix_error_while ctx files f =
  Lwt.catch f begin function
    | Unix.Unix_error (code, f, a) ->
        raise @@ File.unix_error ~ctx files (code, f, a)
    | e ->
        Lwt.reraise e
  end

let descriptor f =
  Lwt_unix.openfile (File.absname f)

let exists f =
  Lwt_unix.file_exists (File.absname f)

let misses f =
  let* p = exists f in
  Lwt.return (not p)

let stat f =
  Lwt_unix.stat (File.absname f)

let link ?dry src dst =
  let* exists = exists dst in
  if not exists &&
     not (File.err_if ?dry "Missing link `%a'" File.print dst)
  then begin
    Log.debug "link: `%s' -> `%s'" (File.name src) (File.name dst);
    catch_unix_error_while "linking" [A src; A dst] begin fun () ->
      Lwt_unix.link (File.absname src) (File.absname dst)
    end
  end else Lwt.return ()

let unlink file =
  catch_unix_error_while "removing" [A file] begin fun () ->
    Lwt_unix.unlink (File.absname file)
  end

let move_in ~dir file =
  link file (File.assume_in ~dir @@ File.basename file) >>= fun () ->
  unlink file

(** Tells whether [f2] exists and has a modification time more recent than
    [this] (which must exist). *)
let newer_than ~this f2 =
  let* this_stat = stat this in
  Lwt.catch begin fun () ->
    let* f2_stat = stat f2 in
    Lwt.return (f2_stat.st_mtime > this_stat.st_mtime)
  end begin function
    | Unix.Unix_error (ENOENT, _, _) ->
        Lwt.return false
    | e ->
        Lwt.reraise e
  end

let touch_dir (dir: File.dir) : unit Lwt.t =
  let rec create dir =
    let dirname = File.absname dir in
    let* exists = Lwt_unix.file_exists dirname in
    if exists && Sys.is_directory dirname
    then Lwt.return ()
    else begin
      let* () = create (File.assume @@ Filename.dirname dirname) in
      Lwt.catch begin fun () ->
        let* () = Lwt_unix.mkdir (File.name dir) 0o755 in
        Log.LWT.debug "Created directory `%a'" File.print dir
      end begin function
        | Unix.Unix_error (EEXIST, _, _) ->
            Lwt.return ()     (* ignore (possible due to concurrent calls). *)
        | e ->
            Lwt.reraise e
      end
    end
  in
  catch_unix_error_while "creating directory" [A dir] begin fun () ->
    create dir
  end

let mkdir dir =
  let dir = File.from_name dir in
  let* () = touch_dir dir in
  Lwt.return dir

let dir_is_empty d =
  catch_unix_error_while "inspecting directory" [A d] begin fun () ->
    let* dh = Lwt_unix.opendir (File.absname d) in
    let* dentry = Lwt_unix.readdir_n dh 3 in  (* contains .. and . by default *)
    Lwt.return (Array.length dentry <= 2)
  end

let unwrap_unix_error_from_stream ~ctx files stream =
  Lwt_stream.wrap_exn stream |>
  Lwt_stream.map begin function
    | Ok e -> e
    | Error Unix.Unix_error (code, f, a) ->
        raise @@ File.unix_error ~ctx files (code, f, a)
    | Error e ->
        Lwt.reraise e
  end

let files_of_dir (d: File.dir) =
  let dir = File.absname d in
  Lwt_unix.files_of_directory dir |>
  Lwt_stream.map (fun f -> File.from_name @@ Filename.concat dir f) |>
  Lwt_stream.filter (fun f -> not (Sys.is_directory @@ File.absname f)) |>
  unwrap_unix_error_from_stream ~ctx:"listing files in" [A d]

let dirs_of_dir (d: File.dir) =
  let dir = File.absname d in
  Lwt_unix.files_of_directory dir |>
  Lwt_stream.map (fun f -> File.from_name @@ Filename.concat dir f) |>
  Lwt_stream.filter (fun f -> Sys.is_directory @@ File.absname f)

let unlink_files_of_dir dir =
  catch_unix_error_while "unlinking files from" [A dir] begin fun () ->
    files_of_dir dir |>
    Lwt_stream.iter_s unlink              (* catch errors on individual files *)
  end

(* Directory-wide locks *)

type dirlock_operation = Read | ReadWrite

let with_lock_in dir (operation: dirlock_operation) f =
  let doit () =
    catch_unix_error_while "trying to lock" [A dir] begin fun () ->
      let* () = touch_dir dir in
      let* lockfile_descr =
        Lwt_unix.openfile (Filename.concat (File.absname dir) ".lock")
          [O_CREAT; O_RDWR] 0o644
      in
      let lop = Lwt_unix.(if operation = Read then F_RLOCK else F_LOCK) in
      let* () = Lwt_unix.lockf lockfile_descr lop 0 in
      let* res =
        Lwt.catch f begin fun exn ->
          let* () = Lwt_unix.lockf lockfile_descr F_ULOCK 0 in
          let* () = Lwt_unix.close lockfile_descr in
          Lwt.reraise exn
        end
      in
      let* () = Lwt_unix.lockf lockfile_descr F_ULOCK 0 in
      let* () = Lwt_unix.close lockfile_descr in
      Lwt.return res
    end
  in
  match operation with   (* FIXME: we'd need to count re-read by same process *)
  | Read ->
      doit ()
  | ReadWrite ->
      Lwt_lock.ez_processwide ~ident:(File.absname dir) doit

(* Management of symbolic links *)

let resymlink ~to_dir ~src ~dst =
  let rec aux () =
    Lwt.catch begin fun () ->
      Lwt_unix.symlink ~to_dir src dst
    end begin function
      | Unix.Unix_error (EEXIST, _, _) ->
          Lwt.bind (Lwt_unix.unlink dst) aux
      | e ->
          Lwt.reraise e
    end
  in
  catch_unix_error_while "creating symbolic link to" [A (File.from_name src)]
    aux

let newsymlink ~to_dir ~src ~dstformat =
  let rec attempt ?(n = 1) () =
    let* r =
      Lwt.catch begin fun () ->
        let link = dstformat n in
        let* () = Lwt_unix.symlink ~to_dir src link in
        Lwt.return (Ok link)
      end begin function
        | Unix.Unix_error (EEXIST, _, _) ->
            Lwt.return (Error ())
        | e ->
            Lwt.reraise e
      end
    in
    match r with
    | Ok link -> Lwt.return link
    | Error () -> attempt ~n:(succ n) ()
  in
  catch_unix_error_while "creating symbolic link to" [A (File.from_name src)]
    attempt

let redirect_symlinks_of ~dir ~src ~dst =
  catch_unix_error_while "redirecting symbolic links of" [A dir] begin fun () ->
    let dir = File.absname dir and dst = File.basename dst in
    Lwt_unix.files_of_directory dir |>
    Lwt_stream.iter begin fun f ->
      let f = Filename.concat dir f in
      if try Unix.readlink f = src with Unix.Unix_error _ -> false then begin
        Unix.unlink f;
        Unix.symlink dst f
      end
    end
  end

(* Syntax *)

module Syntax = struct
  let with_file ~mode ~flags file f =
    catch_unix_error_while "opening" [A file] begin fun () ->
      Lwt_io.with_file ~mode ~flags (File.absname file) f
    end
  let (let<*) file f =
    with_file file f ~mode:Input ~flags:[O_RDONLY; O_NONBLOCK]
  let (let>*) file f =
    with_file file f ~mode:Output ~flags:[O_CREAT; O_WRONLY; O_TRUNC;
                                          O_NONBLOCK]
  let (let>>*) file f =
    with_file file f ~mode:Output ~flags:[O_CREAT; O_WRONLY; O_APPEND;
                                          O_NONBLOCK]
  let (let>*%)   file f
    = (let>*)    file (fun c -> f @@ Lwt_fmt.of_channel c)
  let (let>*%!)  file f
    = (let>*%)   file (fun c -> f @@ Lwt_fmt.get_formatter c)
  let (let>>*%)  file f
    = (let>>*)   file (fun c -> f @@ Lwt_fmt.of_channel c)
  let (let>>*%!) file f
    = (let>>*%)  file (fun c -> f @@ Lwt_fmt.get_formatter c)
end

let empty file =
  Lwt.catch begin fun () ->
    Lwt_io.with_file ~mode:Output ~flags:[O_CREAT; O_WRONLY; O_NONBLOCK; O_EXCL]
      (File.absname file) (fun _oc -> Lwt.return ())
  end begin function
    | Unix.Unix_error (EEXIST, _, _) ->
        raise @@ File.unexpected file
    | Unix.Unix_error (code, f, a) ->
        raise @@ File.unix_error ~ctx:"creating empty file" [A file] (code, f, a)
    | e ->
        Lwt.reraise e
  end

let touch file =
  let open Syntax in
  let>>* _ = file in                               (* do nothing if it exists *)
  Lwt.return ()

let copy_in ~dir file =
  let open Syntax in
  let file_res = File.assume_in ~dir @@ File.basename file in
  catch_unix_error_while "copying" [A file] begin fun () ->
    let* () =
      let<* ic = file in
      let>* oc = file_res in
      Lwt_io.read_lines ic |> Lwt_io.write_lines oc
    in
    Lwt.return file_res
  end

(* String-based manipulations *)

let read f =
  let open Syntax in
  let<* ic = f in
  Lwt_io.read ic

let write f s =
  let open Syntax in
  let>* oc = f in
  Lwt_io.write oc s

let digest f =
  Lwt.map Digest.string (read f)
