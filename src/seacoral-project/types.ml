(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Types used to represent and manipulate Seacoral projects. *)

open Sc_sys.File.TYPES

(** A project-specific problem actually corresponds to an Ltest configuration *)
type project_problem =
  Sc_ltest.Types.config

(** Configuration given to the project *)
type project_config =
  {
    project_run: run;                                      (** runtime status *)
    project_workspace: Sc_core.Types.workspace;            (** root workspace *)
    project_check_syntax: bool; (** check for syntax errors prior to labeling *)
    project_problem: project_problem;      (** actual test generation problem *)
    project_pointer_handling: pointer_handling; (** parameters for interpreting
                                                    pointer *)
    project_test_timeout: float;               (** timeout for each test *)
    project_max_validation_concurrency: int;
    project_verbose_validation: bool;
    project_srcdir_root: dir;                  (** root of input source files *)
  }

(** Pointer-specific configuration *)
and pointer_handling =
  {
    treat_pointer_as_array: Sc_C.Types.named_location list;
    treat_pointer_as_cstring: Sc_C.Types.named_location list;
    array_size_mapping: Sc_C.Types.named_loc_assoc list;
  }

(** Info on previous and current runs. *)
and run =
  {
    run_num: int;             (** Number of Seacoral runs on the same problem *)
    run_ref_times: float NEL.t;         (** Reference time of the current run *)
    run_configs: [`toml] file list;       (** TOML files with actual configs for
                                              previous and current runs (most
                                              recent last); |[run_configs]| =
                                              [run_num]. *)
  }

(** Project parameters, computed from the project configuration. *)
type 'raw_test project_params =
  {
    cil: Cil.file;             (** CIL representation for the whole C project *)
    cil_typing_info: c_types_info;
    typdecls: Sc_values.typdecls;   (** type declarations, derived from [cil] *)
    func_repr: Sc_C.Types.func_repr;        (** representation of the entrypoint
                                                function; guaranteed not to be
                                                `main` *)
    test_struct: Sc_values.Struct.typ;   (** [struct] type that gathers all test
                                             inputs for the entrypoing *)
    test_repr: 'raw_test Sc_corpus.Types.repr; (** module for test inputs
                                                   representation *)
    init_func: Sc_C.Types.func_repr option;     (** optional test initializer *)
    oracle_func: Sc_C.Types.func_repr option;   (** optional test oracle *)
    seek_oracle_failures: bool;           (** whether analysis tools should seek
                                              tests that fail the oracle *)
  }

and c_types_info =
  {
    stdbool_needed: bool; (** Indicates whether the C code parsed as CIL
                              requires `stdbool.h` *)
  }

and any_project_params = A: _ project_params -> any_project_params

(** A projects' "internal" data, parameterized with the type of stores for
    handling inputs. *)
type 'raw_test project =
  {
    config: project_config;               (** given configuration *)
    params: 'raw_test project_params;     (** computed parameters *)
    workspace: Sc_core.Types.workspace;   (** (== [config.project_workspace]) *)
    codebase: [`C] file;             (** Never defines or use symbol `main` *)
    types_header: [`h] file;         (** C header file with declaration of types
                                         from the codebase *)
    func_header: [`h] file;                (** C header file with declaration of
                                               entrypoint and globals *)
    labels: Sc_ltest.Types.labels; (** managed labels (could be in parameters) *)
    label_data: Sc_ltest.Types.label_data;     (** managed label data *)
    store: Sc_store.t;                         (** store of coverage statuses *)
    decoder: 'raw_test Sc_corpus.Decoder.t;    (** dynamic raw test decoder *)
    validator: 'raw_test Sc_corpus.Validator.t;
    corpus: 'raw_test Sc_corpus.corpus;                  (** corpus of inputs *)
    outdir: dir;                   (** output directory for generated C files *)
    covdir: dir;                   (** output directory for label-covering C
                                       files (subdirectory of [outdir]) *)
    rtedir: dir; (** output directory for RTE-triggering C files (subdirectory
                     of [outdir]) *)
    faildir: dir option; (** output directory for C files that fail the oracle
                             (subdirectory of [outdir]) *)
    stats: statistics;                            (** various tool statistics *)
    extra: project_extra;     (** additional data used for project management *)
  }

and statistics = Statistics.t

and project_extra =
  {
    given_entrypoint_name: string; (** name of the entrypoint originally given
                                       (may be "main") *)
  }

(** Succinct information about the project (coverage status, corpus info). *)
type info = Sc_store.Types.covinfo * Sc_corpus.Types.info

(** Tool statistics *)
type tool_stats = Statistics.tool_stats =
  {
    time: float;
    status: (unit, string) Result.t;                 (* TODO: anything better *)
    tests_generated: int;
  }

type testsuite =
  | Split_testsuite of
      {
        cov: individual_test_in_testsuite list;
        covdir: dir;
        rte: individual_test_in_testsuite list;
        rtedir: dir;
        fail: individual_test_in_testsuite list option;
        faildir: dir option;
      }
  | Combined_testsuite of
      {
        cov: combined_tests_in_testsuite;
        covdir: dir;
        rte: combined_tests_in_testsuite;
        rtedir: dir;
        fail: combined_tests_in_testsuite option;
        faildir: dir option;
      }

and individual_test_in_testsuite =
  {
    file: [`C] file;
    metadata: Sc_corpus.Types.test_metadata;
  }

and combined_tests_in_testsuite =
  {
    file: [`C] file;
    metadata: Sc_corpus.Types.test_metadata list;
  }

(** {2 Errors & exceptions} *)

type setup_error = Sc_ltest.Types.error                            (* for now *)

exception SETUP_ERROR of setup_error

type labeling_error =
  | Failed_labeling of
      exn Sc_core.Types.generic_preprocessing_error
  | Syntax_errors of
      {
        cmd_error: Sc_C.Cmd.error;
        stderr_lines: string list;
      }

exception LABELING_ERROR of labeling_error

type elaboration_error =
  | Failed_elaboration of
      exn
  | Unknown_formals of
      {
        formals: Basics.Strings.t;                               (* non-empty *)
        func: Sc_C.Types.func_repr;
      }
  | Unknown_function of
      {
        fun_name: string;
        c_file: [`C] file option;
      }
  | Unsupported_formals of
      {
        formals: Basics.Strings.t;                               (* non-empty *)
        func: Sc_C.Types.func_repr;
      }
  | Unexpected_initialization_function_with_args of
      {
        func: Sc_C.Types.func_repr;
      }

exception ELABORATION_ERROR of elaboration_error
