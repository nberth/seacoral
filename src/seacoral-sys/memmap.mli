(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** Memory-mapping of files

    {b Warning}: Use {!Lwt_memmap} in LWT-based programs; otherwise some
    underlying calls to read or write barriers may fail with a {!Unix.EINTR}
    error (signal with handler received during a pending call to
    [Unix.lockf]). *)

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

(** [bind ~file ~cell_kind ~size ~access] binds [file] to a bigarray with [size]
    cells of type [cell_kind].  [access] describes the operations that may be
    performed on the resulting array: writing to the array is only permitted
    with [ReadWrite] access.  A lock is also attached to [file], that permits
    concurrent accesses with other processes.

    If successfull, this function returns a key that provides access to the
    underlying array (via functions {!read_readonly}, {!read}, or {!write}
    below), along with a finalization function that closes the file.  For the
    moment, the lock is always acquired for the entire file at once whenever any
    of the three aforementioned functions is called.

    Example usage:
    {[
      let bars, finalize =
        Sc_sys.Memmap.bind ~file ~access:ReadOnly ~size
          ~cell_kind:Bigarray.int8_unsigned
      in
      let cell0 =
        let||> array = bars in
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
  -> 'key * (unit -> unit)

val read_readonly: 'a read_only -> f: ('a -> 'b) -> 'b
val read: 'a read_write -> f: ('a -> 'b) -> 'b
val write: 'a read_write -> f: ('a -> 'b) -> 'b

module Syntax: sig
  val (let|>)  : 'a read_only -> ('a -> 'b) -> 'b
  val (let||>) : 'a read_write -> ('a -> 'b) -> 'b
  val (let<||) : 'a read_write -> ('a -> 'b) -> 'b
end
