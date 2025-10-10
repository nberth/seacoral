(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** Helpers for Cil datatypes manipulation. *)

open Basics
open Types

(** {2 Variable declarations} *)

val var_name: var -> string
val var_names: vars -> string list

(** [varset vars] is the set of all variables names in [vars] *)
val varset: vars -> Strings.t

val typ : var -> Cil.typ

(** {2 Attributes} *)

(** Returns details on the attributes of an array {e type}. *)
val carray_type_attributes: Cil.attributes -> carray_type_attrs

val as_pointer_to_carray: ?size:[`Int of int | `Var of string] -> var -> var
val as_pointer_to_cstring: var -> var

(** [pointer_validity attrs] retrieves the kind of a pointer {e variable} or {e
    field} from its attributes [attrs]: it is either a regular C array (the
    default when no specific attrribute is given), or a C string
    (null-terminated). *)
val pointer_validity: Cil.attributes -> pointer_validity

val const_attribute: Cil.attributes -> bool
val const_typ: Cil.typ -> bool

(** {2 Accessors and predicates} *)

exception ARG_INFO_ERROR of Cil.typ

(** [fun_arg_names f file] returns the names of [f]'s formal arguments. *)
val fun_arg_names: string -> Cil.file -> string list

(** [fun_arg_typs f file] returns the type of [f]'s formal arguments. *)
val fun_arg_typs: string -> Cil.file -> Cil.typ list

(** [fun_arg_unrolled_typs f file] same as [fun_arg_typs], but unrolls types. *)
val fun_arg_unrolled_typs: string -> Cil.file -> Cil.typ list

(** [fun_args f file] returns the name and type of [f]'s formal arguments. *)
val fun_args: string -> Cil.file -> vars

(** [fun_unrolled_args f file] same as [fun_args], but unrolls types. *)
val fun_unrolled_args: string -> Cil.file -> vars

(** [fun_return_typ f file] returns [f]'s return type. *)
val fun_return_typ: string -> Cil.file -> Cil.typ

(** [glob_types file]
    Returns the type of the global variables *)
val glob_types: Cil.file -> Cil.typ list

(** [glob_unrolled_types file]
    Same as [glob_types] but unrolls types *)
val glob_unrolled_types: Cil.file -> Cil.typ list

(** [glob_types file]
    Returns the name of the global variables *)
val glob_names: Cil.file -> string list

(** [glob_infos file]
    Returns the type and the name of the global variables *)
val glob_infos: Cil.file -> vars

(** [glob_unrolled_infos file]
    Same as [glob_infos] but unrolls types *)
val glob_unrolled_infos: Cil.file -> vars

(** [fold_typ_defs file f acc] folds [f] over every type definition in
   [file]. *)
val fold_typ_defs: Cil.file -> (Cil.global -> 'a -> 'a) -> 'a -> 'a

(** [diff_globals cil1 cil2] Performs a diff between [cil1] and [cil2] by
    relying on source file locations to check equality.  *)
val diff_globals: Cil.file -> Cil.file -> Cil.file

(* Other stuff: *)

(** Returns the list of subarrays sizes of the array type in argument.
    If the type is not an array, returns [] *)
val multidim_array_sizes: Cil.typ -> carray_type_attrs list

(** Initializes the global variables of a file. For now, it ignores the
    [for_func] parameter. *)
val func_env: ?for_func:string -> Cil.file -> func_env

val map_func_inputs: f:(input_kind -> var -> var) -> func_repr -> func_repr

(** [cil_func file f]

    Builds the simplified representation of the function [f] defined in file
    [file]. *)
val cil_func: Cil.file -> string -> func_repr

val func_inputs: func_repr -> vars

val parse_file: [`C] Sc_sys.File.file -> Cil.file
