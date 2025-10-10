(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** Basic primitives for accessing a memory-mapped files *)

open File.TYPES

open Lwt.Syntax

(* --- *)

module TYPES = struct

  type ('a, 'res) read_barrier =
    with_read:('a -> 'res Lwt.t) -> 'res Lwt.t

  type ('a, 'res) read_n_write_barrier =
    with_read_n_write:('a -> 'res Lwt.t) -> 'res Lwt.t

  type ('a, _) access =
    | ReadOnly: ('a, 'a read_only) access
    | ReadWrite: ('a, 'a read_write) access
  and 'a read_only =
    { with_read_access: 'res. ('a, 'res) read_barrier }
  and 'a read_write =
    { with_read_access: 'res. ('a, 'res) read_barrier;
      with_read_n_write_access: 'res. ('a, 'res) read_n_write_barrier }

end
include TYPES

(* --- *)

let open_bin (file : [`bin] file) =
  let* fd = Lwt_file.descriptor file [O_RDWR; O_CREAT] 0o600 in
  Lwt.return (fd, fun () -> Lwt_unix.close fd)

let mmap fd ~cell_kind size =
  let shared = true
  and fd = Lwt_unix.unix_file_descr fd in
  let rec domap () =
    try
      Lwt.return @@
      Unix.map_file fd cell_kind Bigarray.c_layout shared [|size|]
    with Unix.Unix_error (EINTR, _, _) ->    (* signal handled during syscall… *)
      domap ()                              (* we can try again *)
  in
  domap ()

(* --- *)

let access_barriers (type a e) fd barray
  : ((int, e, Bigarray.c_layout) Bigarray.Array1.t, a) access -> a Lwt.t =
  let array = Bigarray.array1_of_genarray barray in
  let unlock () = Lwt_unix.lockf fd F_ULOCK 0 in
  let rlock () = Lwt_unix.lockf fd F_RLOCK 0
  and rwlock () = Lwt_unix.lockf fd F_LOCK 0 in
  let protect ~lock ~f =
    let* () = lock () in
    Lwt.catch begin fun () ->
      let* res = f array in
      let* () = unlock () in
      Lwt.return res
    end begin fun e ->
      let* () = unlock () in
      Lwt.reraise e
    end
  in
  let readb ~with_read:f = protect ~lock:rlock ~f
  and rwb ~with_read_n_write:f = protect ~lock:rwlock ~f in
  function
  | ReadOnly -> Lwt.return { with_read_access = readb }
  | ReadWrite -> Lwt.return { with_read_access = readb;
                              with_read_n_write_access = rwb }

let bind (type a e)
  : file: [`bin] file -> cell_kind: (int, e) Bigarray.kind -> size: int
    -> access: ((int, e, Bigarray.c_layout) Bigarray.Array1.t, a) access
    -> (a * (unit -> unit Lwt.t)) Lwt.t =
  fun ~file ~cell_kind ~size ~access ->
  let* fd, finalize = open_bin file in
  let* barray =
    Lwt.catch begin fun () ->
      mmap fd ~cell_kind size
    end begin fun e ->
      let* () = Lwt_unix.close fd in
      Lwt.reraise e
    end
  in
  let* access = access_barriers fd barray access in
  Lwt.return (access, finalize)

(* --- *)

let read_readonly ({ with_read_access }: _ read_only) ~f =
  with_read_access ~with_read:f

let read ({ with_read_access; _ }: _ read_write) ~f =
  read_readonly { with_read_access } ~f

let write ({ with_read_n_write_access; _ }: _ read_write) ~f =
  with_read_n_write_access ~with_read_n_write:f

module Syntax = struct
  let (let|>*) b f = read_readonly b ~f
  let (let||>*) b f = read b ~f
  let (let<||*) b f = write b ~f
end
