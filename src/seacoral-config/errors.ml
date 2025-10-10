(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

exception Unknown_toml_key of string

exception Bad_value_type_in_toml_table of
    Toml.Types.Table.Key.t *                  (* Key of the toml table *)
    Toml.Types.value *                        (* Toml value  *)
    string                                    (* description of expected type *)

exception Bad_value_in_toml_table of
    {
      key: Toml.Types.Table.Key.t;
      value: Toml.Types.value;
      expect: Basics.PPrt.ufmt;
      pp_doc: Format.formatter -> unit;
    }

(* Raised when requesting a configuration section that has not been loaded. *)
exception Unconfigured_section of string

(* Raised when trying to register two sections with the same name. *)
exception Duplicate_section_names of string

exception Invalid_string_value of
    {
      key: string;
      value: string;
      descr: string option;
    }

type toml_error =
  | TOML_error of
      {
        toml: string;                   (** Actualy contents of the TOML file *)
        msg: string;                    (** Error message *)
        loc: Toml.Parser.location;      (** Location of the error *)
      }
