(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Sc_sys.File.TYPES

open Lwt.Syntax
open Sc_sys.File.Syntax

module type Crunched = sig
  val file_list : string list
  val read : string -> string option
  val hash : string -> string option
  val size : string -> int option
end

(* TODO: check its hash *)
let file_already_exported ~hash f =
  ignore hash;
  Sc_sys.File.exists f

let deflate_file_in ~(dir: dir) (module C: Crunched) (file: string) : unit =
  let file_content =
    match C.read file with
    | None -> Fmt.invalid_arg "%s.deflate_in" __MODULE__
    | Some f -> f
  in
  let f = Sc_sys.File.assume_in ~dir file in
  Sc_sys.File.touch_dir (Sc_sys.File.parent f);
  if not @@ file_already_exported ~hash:(C.hash file) f then begin
    let () = Sc_sys.File.touch f in
    let> chan = f in
    output_string chan file_content
  end

let deflate_in ~(dir: dir) (module C: Crunched) : unit =
  Sc_sys.File.touch_dir dir;
  List.iter (deflate_file_in (module C) ~dir) C.file_list

module LWT = struct

  let file_already_exported ~hash f =
    Lwt.catch begin fun () ->
      Lwt_io.with_file ~mode:Input (Sc_sys.File.name f) begin fun ic ->
        let* contents = Lwt_io.read ic in
        Lwt.return (Digest.string contents = hash)
      end
    end begin function
      | Unix.Unix_error (ENOENT, _, _) ->
          Lwt.return false
      | e ->
          Lwt.reraise e
    end

  let deflate_file_in ~dir (module C: Crunched) file : unit Lwt.t =
    let file_content, hash =
      match C.read file, C.hash file with
      | Some f, Some h ->
          f, h
      | None, _ | _, None ->
          Fmt.invalid_arg "%s.deflate_in" __MODULE__              (* Not_found? *)
    in
    let f = dir / file in
    let* ok = file_already_exported ~hash f in
    if ok then Lwt.return_unit else begin
      let* () = Sc_sys.Lwt_file.touch_dir (Sc_sys.File.parent f) in
      Lwt_io.with_file ~mode:Output (Sc_sys.File.name f) begin fun oc ->
        Lwt_io.write oc file_content
      end
    end

  let deflate_in ~dir (module C: Crunched) : unit Lwt.t =
    let* () = Sc_sys.Lwt_file.touch_dir dir in
    Lwt_list.iter_p (deflate_file_in ~dir (module C)) C.file_list

  let deflate_once_in: dir: dir -> ident: string -> _ -> unit Lwt.t =
    let do_once: ident: (dir * string) -> (unit -> unit Lwt.t) -> unit Lwt.t =
      Sc_sys.Lwt_lazy.do_once ~keep_idents:true (module struct
        type t = dir * string
        let equal = (=)
        let hash = Hashtbl.hash
      end)
    in
    fun ~dir ~ident res ->
      do_once ~ident:(dir, ident) begin fun () ->
        deflate_in ~dir res
      end

end
