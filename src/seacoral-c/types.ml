(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Helpers for Cil datatypes manipulation. *)

(** {2 Variable declarations} *)

type var = Cil.typ * string * Cil.attributes
type vars = var list

(** {2 Access path} *)

(** A concrete access path is built out of simple access operations *)
type access_path_operation =
  | Field_access of string     (** access to a field given by its name *)
  | Index_access of int        (** access to an array cell given by its index *)

(** An access path suffix is a non-empty list of access path operations *)
type access_path_suffix = access_path_operation NEL.t

(** A concrete access path combines an origin variable name and/or a suffix *)
type access_path =
  | Access_path_origin of
      string
  | Access_path_suffix of
      access_path_suffix
  | Access_path of
      {
        origin: string;
        suffix: access_path_suffix;
      }

(** {2 Attributes} *)

(** C array {e type} attributes.

    Array attributes give its length and its static/dynamic characteristics. *)
type carray_type_attrs =
  | Static_array of int
  | Dynamic_array

(** {2 Pointer validity} *)

type pointer_validity =
  [ `Carray_with_bound_length of int
  | `Carray_with_length_field of string
  | `Carray_with_unknown_length
  | `Cstring ]

(** {2 Functions} *)

(** The environment of a function. For now, only gathers the file-global
    variables. *)
type func_env =
  {
    glob_vars: vars;
  }

(** The simplified representation of a {!Cil} function. *)
type func_repr =
  {
    func_name: string;
    func_env: func_env;
    func_rtyp: Cil.typ;
    func_args: vars;
  }

type input_kind = Formal | Global

(** {2 Coverage labels} *)

type cov_label_view =
  {
    cov_label_id: int;
    (** The unique ID of the proof objective *)

    cov_label_status: cov_label_status;
    (** The current status of the proof objective *)

    cov_label_orig_loc: location;
    (** The location of the proof objective in the original file *)

    cov_label_loc: location;
    (** The location in the labelized file *)

    cov_label_emitter: string option;
    (** The tool that emited the status *)

    cov_label_exec_time: float;
    (** Exec time of the proof *)
  }

(** A location is a line in a file. *)
and location =
  {
    loc_file : string;
    loc_line : int;
  }

(** Status of a coverage label / proof objective *)
and cov_label_status =
  | Unknown
  | Covered of string list
  | Uncoverable

type simple = [`simple]
type hyper = [`hyper]
type any = [simple | hyper | `any]
type _ cov_label =
  | S: cov_label_view -> [>simple] cov_label
  | H: cov_label_view -> [>hyper]  cov_label
  | Any: [simple | hyper] cov_label -> [>`any] cov_label

(* --- *)

(** Representation for sets of pointer references *)
type pointer_refs =
  pointer_ref list

(** Type of pointer references *)
and pointer_ref =
  | Variable of
      {
        pointer_var: string;
      }                                   (** Reference to a pointer variable *)
  | Struct_field of
      {
        struct_name: string;
        pointer_field_name: string;
      }                                      (** Reference to a pointer field *)

(** Representation for pointer constraints *)
type pointer_constraints =
  pointer_constraint list

(** A single pointer constraint relates distinct variables or fields in the same
    structure. *)
and pointer_constraint =
  | Distinct_variables of
      {
        pointer_var: string;    (** Name of the constrained pointer variable *)
        size_var: string;       (** Name of the constraining size variable *)
      }                         (** Constraint between two distinct variables *)
  | From_same_struct of
      {
        struct_name: string;        (** The structure type name *)
        pointer_field_name: string; (** Name of the constrained pointer field *)
        size_field_name: string;    (** Name of the constraining size field *)
      }               (** Constraint between two fields of the same structure *)

(** {2 Errors and exceptions} *)

type error =
  | Syntax_error of { expected: parsed_item; string: string }

and parsed_item =
  | Pointer_reference
  | Pointer_constraint

exception Unknown_function of string
exception Invalid_attribute_payload of Cil.attribute
exception Incompatible_attributes of string list
exception Missing_attribute of { name: string; reason: string }

exception UNSUPPORTED_TYPE of Ctypes_static.boxed_typ
