(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** Types for interactions with LTest *)

open Sc_sys.File.TYPES

(** LTest problem definition: these data alone are anough to define a
    test-generation problem. *)
type config =
  {
    input_files: [`C] file list;  (** given C files *)
    entrypoint: string;           (** single entrypoint function to be tested *)
    criterion: string;      (** coverage criterion (see frama-c -lannot-list) *)
    header_dirs: dir list;  (** C header directories *)
    external_libs: string list;                    (** external library names *)
    annotated_functions:
      [`All | `Only of string list | `Auto];   (** specification of functions to
                                                   consider for coverage *)
    ignored_globals: Basics.Strings.t;
    fixtures_files: [`C] file list;              (** additional fixture files *)
    initialization_function: string;
    oracle_function: string;
    seek_oracle_failures: bool;           (** whether analysis tools should seek
                                              tests that fail the oracle *)
  }

(** Module representation of configurations (used internally) *)
module type CONFIG = sig
  val workspace: Sc_core.Types.workspace
  val config: config
end

(** The data managed by LTest. *)
type label_data =
  {
    label_file: [`labeldb] file;       (** csv file containing information about
                                           the label coverage/uncoverability. *)
    labelized_file: [`C | `labelized] file; (** C file with the extra labels. *)
  }

(** Labels distinguised based on their respective familly *)
type labels =
  {
    simpl: Sc_C.Cov_label.simple list;       (** simple labels *)
    hyper: Sc_C.Cov_label.hyper list;        (** hyper labels *)
    any: Sc_C.Cov_label.any list;            (** concatenation of all labels. *)
  }

type lreplay_results =
  {
    lreplay_covered: Basics.Ints.t;
    lreplay_uncoverable: Basics.Ints.t;
    lreplay_unknown: Basics.Ints.t;
    lreplay_labels: Sc_C.Cov_label.simple list;
  }

type lreplay_display_style = Compact | One_per_line

(** Errors *)

type error =
  | Missing_frama_c_plugin of { name: string }
  | Unsupported_criterion of { crit: string; supported_criteria: string list }
  | Unknown_criterion of { crit: string; supported_criteria: string list }

exception ERROR of error
