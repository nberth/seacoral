(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025-2026 OCamlPro                                      *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Operations on access paths *)

type t = Types.access_path

type suffix = Types.access_path_suffix

(** {2 Printing} *)

val print: t Fmt.t
val print_suffix: suffix Fmt.t
val to_string : t -> string

(** {2 Construction} *)

val field: string -> suffix
val index: int -> suffix

val from: string -> suffix option -> t
val append_to_suffix: suffix -> suffix -> suffix
val append_to_suffix': suffix option -> suffix -> suffix
val origin_only: string -> t
val suffix_only: suffix -> t
val append: t -> suffix -> t
val append': t -> suffix option -> t
val append_field : t -> string -> t
val append_index : t -> int -> t

(** {2 Destruction} *)

val origin : t -> string option
val origin' : t -> string

val suffix : t -> suffix option
val suffix' : t -> suffix

(** {2 Miscellaneous} *)
val compare : t -> t -> int

(** {2 Substitutions} *)

val subst_rightmost_suffix_element: suffix -> suffix -> suffix
val subst_rightmost_suffix: t -> suffix -> t

(** Various (hackish / brittle) stuff *)
module HACK: sig
  val forget_first_suffix_punct: suffix -> t
  val shift_left: t -> t
end

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
