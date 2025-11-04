(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Sc_sys.File.TYPES

module type ToHtml = sig
  type t
  val to_html : t -> string
  val to_fhtml : Format.formatter -> t -> unit
end

type 'content accordion

type 'content tab

val make_accordion_list:
  name: string ->
  (string * 'a) list ->
  'a accordion list

val make_tab_list:
  name: string ->
  (string * 'a) list ->
  'a tab list

(** A tool configuration is a list of pairs (key * value). *)
type tool_config = (string * string) list

(** A run configuration is a list of tool configurations. *)
type run_config = tool_config accordion list

type run = {
  run_time: float;
  (** The time of the run. *)

  run_config: run_config;
  (** The run configuration. *)

  extra_run_html: string
  (** Some extra custom HTML. *)
}

(** A list of tabs, each for one run. *)
type run_tabs = run tab list

(** The overview page. *)
type overview = {
  num_tests_gen: int;
  num_tests_imported: int;
  num_labels: int;
  num_covered: int;
  num_uncoverable: int;
  num_runs: int;
  runs: run_tabs
}

(** A single test. *)
type test = {
  tool_name: string;
  (** The tool that generated the test. *)

  tool_time: float;
  (** The time it took to generate the test. *)

  test_file: [`C] file;
  (** The test file name. Will be opened. *)
}

type combined_tests = {
  file: [`C] file;
}

(** A list of tests. *)
type tests =
  | Split of test tab list
  | Combined of combined_tests tab list (* [`C] Sc_sys.File.t *)

(** All the information on a label from the static and runtime databases. *)
type label = {
  static_db_data: Sc_C.Cov_label.simple;
  runtime_db_data: [ `Cov | `Uncov | `Unk ]
}

(** Each possible tab. *)
type report_tab =
  | Overview of overview
  | Tests of tests
  | Labels of label tab list

type report = {
  r_title: string;
  r_tabs: report_tab tab list;
}

module Report : ToHtml with type t = report
