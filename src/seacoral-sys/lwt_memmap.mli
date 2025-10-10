(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** Memory-mapping of files (LWT-ready) *)

module TYPES: sig
  type ('a, _) access =
    | ReadOnly: ('a, 'a read_only) access
    | ReadWrite: ('a, 'a read_write) access
  and 'a read_only
  and 'a read_write
end
include module type of TYPES
  with type 'a read_only = 'a TYPES.read_only
   and type 'a read_write = 'a TYPES.read_write

(** [bind ~file ~cell_kind ~size ~access] is an LWT-based equivalent of
    {!Memmap.bind}.

    Example usage:
    {[
      let open Lwt.Syntax in
      let* bars, finalize =
        Sc_sys.Lwt_memmap.bind ~file ~access:ReadOnly ~size
          ~cell_kind:Bigarray.int8_unsigned
      in
      let* cell0 =
        let||>* array = bars in
        array.{0}
      in
      finalize ()
    ]}
*)
val bind
  : file: [`bin] File.t
  -> cell_kind: (int, 'cell) Bigarray.kind
  -> size: int
  -> access: ((int, 'cell, Bigarray.c_layout) Bigarray.Array1.t, 'key) access
  -> ('key * (unit -> unit Lwt.t)) Lwt.t

val read_readonly: 'a read_only -> f: ('a -> 'res Lwt.t) -> 'res Lwt.t
val read: 'a read_write -> f: ('a -> 'res Lwt.t) -> 'res Lwt.t
val write: 'a read_write -> f: ('a -> 'res Lwt.t) -> 'res Lwt.t

module Syntax: sig
  val (let|>*)  : 'a read_only -> ('a -> 'res Lwt.t) -> 'res Lwt.t
  val (let||>*) : 'a read_write -> ('a -> 'res Lwt.t) -> 'res Lwt.t
  val (let<||*) : 'a read_write -> ('a -> 'res Lwt.t) -> 'res Lwt.t
end
