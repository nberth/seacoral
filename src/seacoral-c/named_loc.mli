(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
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

(** [var_mem v gap_list] holds whenever the size of an array referenced by a
    pointer variable [v] is constrained according to [gap_list]. . *)
val var_mem
  : string
  -> named_location list
  -> bool

(** [find_var ~varname gap_list] returns the name of the variable that holds the
    size of the array referenced by a pointer [varname], if any. *)
val find_var
  : varname: string
  -> named_loc_assoc list
  -> string option

(** [field_mem ~struct_name ~field_name gap_list] returns [true] iff a field of
    a structure [struct_name] holds the size of the array referenced by a
    pointer field [field_name] in the same structure. *)
val field_mem
  : struct_name: string
  -> field_name: string
  -> named_location list
  -> bool

(** [find_field ~struct_name ~field_name gap_list] returns the name of the field
    of any structure [struct_name] that holds the size of the array referenced
    by a pointer field [field_name] in the same structure. *)
val find_field
  : struct_name: string
  -> field_name: string
  -> named_loc_assoc list
  -> string option
