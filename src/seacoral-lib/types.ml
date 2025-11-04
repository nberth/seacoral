(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

type run_config =
  {
    config_input: Sc_config.Eztoml.TYPES.toml_file option;
    (** TOML configuration file, either given via command-line arguments, or
        found automatically based on the current directory *)
    workdir: string;   (** name of directory where all the working files will be
                           stored ("_sc" by default) *)
    force_preprocess: bool; (** force project elaboration, labeling and tool
                                preprocessing stages *)
    enable_syntax_check: bool; (** whether to perform a syntax check prior to
                                   labeling *)
    test_timeout: float;
    tools: string list;                       (** tools to use *)
    strategy: orchestration_strategy;         (** tool orchestration strategy *)
    max_validation_concurrency: int; (** maximum number of validations that may
                                         be performed in parallel *)
    verbose_validation: bool;
  }

(** Orchestration strategy *)
and orchestration_strategy =
  | AllParallel                         (** launch every tool in parallel *)
  | AllSequential                       (** launch tools sequentially *)
  | Optimized                           (** use a heuristic to launch tools *)
  | Custom of Sc_strategy.Types.t       (** use-provided strategy *)

(* --- *)

type project_config =
  {
    name: string;           (** name for the project; may be "" (the default) *)
    input_files: string list;                   (** C files of the project *)
    entrypoint: string;                         (** function to analyze *)
    cover_criterion: string;                    (** Lannot name for criterion *)
    functions_to_cover: [`All | `Only of string list | `Auto];
    header_dirs: string list;       (** dirs where C header files are located *)
    external_libs: string list;
    (* TODO: outdir *)
    ignored_globals: string list;
  }

type fixtures_config =
  {
    files: string list;
    init: string;
    oracle: string;
    seek_oracle_failures: bool;
  }

type pointer_handling_config =
  {
    max_ptr_array_length: int;
    max_non_nil_ptr_depth: int;
    max_cstring_length: int;                   (* excluding a trailing '\000' *)
    treat_pointer_as_array: string list;
    treat_pointer_as_cstring: string list;
    array_size_mapping: string list;
  }

(* --- *)

type config =
  {
    run: run_config;
    project: project_config;
    fixtures: fixtures_config;
    pointer_handling: pointer_handling_config;
  }

type config_error =
  | Missing_input_file
  | Missing_entrypoint
  | Missing_tools
  | Unknown_tools of string NEL.t

exception CONFIG_ERROR of config_error

type tool_preprocessing_error =
  | Failed_preprocessing of
      {
        tool: string;
        error: (exn * Printexc.raw_backtrace)
            Sc_core.Types.generic_preprocessing_error;
      }

exception TOOL_PREPROCESSING_ERROR of tool_preprocessing_error

type tool_computation_error =
  | Failed_computation of
      {
        tool: string;
        error: exn;
        backtrace: Printexc.raw_backtrace;
      }

exception TOOL_COMPUTATION_ERROR of tool_computation_error

type generation_error =
  | Project_setup_error of
      Sc_project.Types.setup_error
  | Project_labeling_error of
      Sc_project.Types.labeling_error
  | Project_elaboration_error of
      Sc_project.Types.elaboration_error
  | Tool_preprocessing_error of
      tool_preprocessing_error
  | Tool_computation_error of
      tool_computation_error

exception GENERATION_ERROR of generation_error
