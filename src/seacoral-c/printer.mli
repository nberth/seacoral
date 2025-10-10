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

open Types

(** {2 Variable declarations} *)

val pp_vars: vars Fmt.t
val pp_formal_decls: vars Fmt.t
val pp_global_decls: vars Fmt.t

(** {2 Access path} *)

val pp_access_path: access_path Fmt.t

(* --- *)

(** [pp_typed_assignment fmt (typ, dest, src)] uses the formatter [fmt] to print
   an assignment from [src] to [dest], the latter being of type [typ]. *)
val pp_typed_assignment: (Cil.typ * string * string) Fmt.t

(** {2 Other stuff for printing and representing functions} *)

(** [string_of_typ ty] Returns the string representation of a type. *)
val string_of_typ: ?enable_static_attr:bool -> Cil.typ -> string

(** Pretty prints an expression. *)
val pp_exp: Cil.exp Fmt.t

(** Pretty prints a type. *)
val pp_typ: Cil.typ Fmt.t

(** Pretty-prints every type definition from a given {!Cil} file. *)
val pp_typ_defs: Cil.file Fmt.t

(** Pretty prints an environment. *)
val pp_func_env: func_env Fmt.t

(** Pretty prints a function representant as an extern C function
    declaration. *)
val pp_fundecl: ?enable_static_attr:bool -> func_repr Fmt.t

(** Pretty prints a named location prefix. *)
val pp_named_location_prefix : Format.formatter -> named_location_prefix -> unit

(** Pretty prints a named location.
    It may be used as a valid C identifier, but pointer accesses with offset
    `'All` are set to the first case of the array ("[0]"). *)
val pp_named_location : Format.formatter -> named_location -> unit

(** Prints a named loc assoc as a pair of named locations. *)
val pp_named_loc_assoc : Format.formatter -> named_loc_assoc -> unit

(* --- *)

(** [emit_testcall ~oracle_assessment ~emit_effective_inputs ~entrypoint
    ~init_func ~oracle_func effective_inputs ppf] emits C code that encapsulates
    a call to the provided [entrypoint] function using the optional functions
    for initialization [init_func] and test oracle [oracle_func].

    If provided, [init_func] should accept the same list of arguments as
    [entrypoint]; any result it returns is ignored.

    If provided, [oracle_func] should accept the same list of arguments as
    [entrypoint], plus an additional parameter with the return type of
    [entrypoint] (if not [void]).  The oracle should also return an integral
    result, which is checked as non-null using a prediacte named
    [oracle_assessment]. *)
val emit_testcall
  : oracle_assessment: string
  -> emit_effective_inputs: 'a Fmt.t
  -> entrypoint: func_repr
  -> ?init_func: func_repr
  -> ?oracle_func: func_repr
  -> 'a
  -> Format.formatter -> unit
