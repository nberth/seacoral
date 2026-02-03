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

val pointer_ref_of_string: string -> pointer_ref
val pointer_constraint_of_string: string -> pointer_constraint

(* --- *)

(** [var_mem v gap_list] holds whenever the size of an array referenced by a
    pointer variable [v] is constrained according to [gap_list]. . *)
val var_mem
  : pointer_var:string
  -> pointer_refs
  -> bool

(** [field_mem ~struct_name ~field_name gap_list] returns [true] iff a field of
    a structure [struct_name] holds the size of the array referenced by a
    pointer field [field_name] in the same structure. *)
val field_mem
  : struct_name: string
  -> pointer_field_name: string
  -> pointer_refs
  -> bool

(* --- *)

(** [find_var ~varname gap_list] returns the name of the variable that holds the
    size of the array referenced by a pointer [varname], if any. *)
val find_var
  : pointer_var: string
  -> pointer_constraints
  -> string option

(** [find_field ~struct_name ~field_name gap_list] returns the name of the field
    of any structure [struct_name] that holds the size of the array referenced
    by a pointer field [field_name] in the same structure. *)
val find_field
  : struct_name: string
  -> pointer_field_name: string
  -> pointer_constraints
  -> string option
