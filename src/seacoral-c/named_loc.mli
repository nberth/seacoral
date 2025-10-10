(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Types

(** Returns the variable as a memory location. *)
val of_varname : ?access_path:abstract_access_path -> string -> named_location

(** Returns Some (varname) if the named location is just a variable name. *)
val if_varname : named_location -> string option

(** Parses a string into a pointer identifier. *)
val of_string : string -> named_location

(** Parses a string into a pair of named locations and merges them into a
    loc association *)
val assoc_of_string : string -> named_loc_assoc

(** [var_mem v l]

    Returns [true] if variable [v] with no access is in [l]. *)
val var_mem : string -> named_location list -> bool

(** [find_assoc ~typname ~varname ~access_path]

    Returns the variable and the access path corresponding to the size of the
    array referenced by [varname] and accessed with [access_path]. *)
val find_assoc :
  varname:string ->
  access_path:abstract_access_path ->
  named_loc_assoc list ->
  (string * abstract_access_path) option

(** [find_assoc' nl nlal]

    Same as [find_assoc], but works with named location. *)
val find_assoc':
  named_location -> named_loc_assoc list -> named_location option
