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

(* TODO: rename what's below *)

type access_path_node =
  | Access_field of string                    (** A field access (arr.field). *)
  | Access_ptr  (** All subarrays of a pointer ( *arr, arr[1]... arr[size-1]) *)

type abstract_access_path = access_path_node list               (* deprecated *)

type named_location_prefix =
  | Variable of string              (** Starting from a specific variable *)
  | Struct of string                (** Starting from any struct of this type *)

(** A named location: can start from a variable or a generic structure type. *)
type named_location =
  {
    prefix: named_location_prefix; (** the variable or structure. *)
    access_path: abstract_access_path; (** how to access the memory location. *)
  }

(** A memory location associated to another variable.
    Used to map arrays to their (variable) size. *)
type named_loc_assoc =
  | Separate_variables of {
    array: named_location;
    (** The array *)
    size: string * abstract_access_path
    (** The variable path of the array. *)
  }
  | From_same_struct of {
      struct_name: string;
      (** The structure type name. *)
      array: abstract_access_path;
      (** How to access the array from a value of type [struct_name]. *)
      size: abstract_access_path;
      (** How to access the arrat size from the same value than for [array]. *)
    }

(** {2 Exceptions} *)

exception Unknown_function of string
exception Invalid_attribute_payload of Cil.attribute
exception Incompatible_attributes of string list
exception Missing_attribute of { name: string; reason: string }

exception UNSUPPORTED_TYPE of Ctypes_static.boxed_typ
