(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Handling of [.ktest] files *)

open Sc_sys.File.TYPES
open Harness.TYPES                                                 (* options *)

type error =
  | Invalid_bout_header of string
  | Unsupported_version of int

exception ERROR of error

val pp_error: error Fmt.t

module type IO = sig
  type raw_test

  (** May raise {!ERROR}, or a system-related exception. *)
  val read
    : [`ktest] file
    -> Sc_values.Struct.typ
    -> options
    -> raw_test Lwt.t

  (** May raise an internal in case of malformed raw test, or a system-related
      exception. *)
  val write
    : [`ktest] file
    -> Sc_values.Struct.typ
    -> options
    -> raw_test
    -> unit Lwt.t
end

module Make_io (Test_repr: Sc_values.Struct.REPR)
  : IO with type raw_test = Test_repr.Val.t
