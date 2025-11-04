(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Coverage labels management *)

open Types

type 'a t = 'a cov_label
type simple = [`simple] t
type hyper  = [`hyper]  t
type nonrec any = any t

(** Returns the unique identifier of the given label. *)
val id: _ t -> int

(** Returns the status of the label. *)
val status: _ t -> cov_label_status

(** Returns the name of the tool who emited a status. *)
val emitter: _ t -> string option

(** Returns the location in the labelized file. *)
val location: _ t -> location

(** Returns true if the label status is 'Covered _' *)
val is_covered: _ t -> bool

(** Returns true if the label status is 'Uncoverable' *)
val is_uncoverable: _ t -> bool

(** Returns true if the label status is 'Unknwon' *)
val is_unknown: _ t -> bool

(** Checks that the labels ids are contiguous and start at 1, then returns the
    max label id (which must be the total number of label).  Raises
    [Invalid_argument] if it does not satisfy the specification. *)
val max_label_id: _ t list -> int

(** {3 Casting} *)

(** As their name/type say. *)

val as_any: _ t -> any

val inspect: _ t -> [`hyper | `simple] t

(** {2 Status} *)

(** The string representation of a status. *)
val show_status: cov_label_status -> string

(** Replaces the label status by the one in argument. *)
val set_status: cov_label_status -> 'a t -> 'a t

(** Prints the string representation of a status. *)
val print_status: cov_label_status Basics.PPrt.pp
