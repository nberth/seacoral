(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Basic primitives for accessing a memory-mapped files *)

open File.TYPES

(* --- *)

module TYPES = struct

  type read_barriers =
    {
      start_reading: unit -> unit;
      stop_reading: unit -> unit;
    }

  type read_n_write_barriers =
    {
      start: unit -> unit;
      stop: unit -> unit;
    }

  type ('a, _) access =
    | ReadOnly: ('a, 'a read_only) access
    | ReadWrite: ('a, 'a read_write) access
  and 'a read_only = read_barriers * 'a
  and 'a read_write = read_barriers * read_n_write_barriers * 'a

end
include TYPES

(* --- *)

let open_bin (file : [`bin] file) =
  let fd = File.descriptor file [O_RDWR; O_CREAT] 0o600 in
  fd, fun () -> Unix.close fd

let mmap fd ~cell_kind size =
  let shared = true in
  Unix.map_file fd cell_kind Bigarray.c_layout shared [|size|]

(* --- *)

let access_barriers (type a e) fd barray
  : ((int, e, Bigarray.c_layout) Bigarray.Array1.t, a) access -> a =
  let array = Bigarray.array1_of_genarray barray in
  let unlock () = Unix.lockf fd F_ULOCK 0
  and rlock () = Unix.lockf fd F_RLOCK 0
  and rwlock () = Unix.lockf fd F_LOCK 0 in
  let readb = { start_reading = rlock; stop_reading = unlock }
  and rwb = { start = rwlock; stop = unlock } in
  function
  | ReadOnly -> readb, array
  | ReadWrite -> readb, rwb, array

let bind (type a e)
  : file: [`bin] file -> cell_kind: (int, e) Bigarray.kind -> size: int
    -> access: ((int, e, Bigarray.c_layout) Bigarray.Array1.t, a) access
    -> a * (unit -> unit) =
  fun ~file ~cell_kind ~size ~access ->
  let fd, finalize = open_bin file in
  let barray =
    try mmap fd ~cell_kind size
    with e ->
      Unix.close fd;
      Lwt.reraise e
  in
  access_barriers fd barray access, finalize

(* --- *)

let read_readonly ((x, arr): _ read_only) ~f =
  x.start_reading ();
  try
    let res = f arr in
    x.stop_reading ();
    res
  with e -> x.stop_reading (); raise e

let read ((x, _, arr): _ read_write) ~f =
  read_readonly (x, arr) ~f

let write ((_, x, arr): _ read_write) ~f =
  x.start ();
  try
    let res = f arr in
    x.stop ();
    res
  with e -> x.stop (); raise e

module Syntax = struct
  let (let|>) b f = read_readonly b ~f
  let (let||>) b f = read b ~f
  let (let<||) b f = write b ~f
end
