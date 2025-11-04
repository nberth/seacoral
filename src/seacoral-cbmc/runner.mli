(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Helpers to run CBMC. *)

open Types

open Sc_sys.File.TYPES

type 'a process_result = 'a                                  (* always provided *)

(** Given a list of proof objectives and properties, returns the properties
    corresponding to unknown proof objectives.  They are matched with their
    location in the C file. *)
val uncovered_properties
  : mode: OPTIONS.mode
  -> harness_file:[`C] file
  -> labelized_file: [`C | `labelized] file
  -> cbmc_props:DATA.property list
  -> already_decided:Basics.Ints.t
  -> labels:Sc_C.Cov_label.simple list
  -> entrypoint:string
  -> [`simple] analysis_env

(** Starts a CBMC process that reads the file and return the list of properties *)
val cbmc_get_properties
  : store: Sc_store.t
  -> runner_options: Types.runner_options
  -> entrypoint:string
  -> files:[`C] Sc_sys.File.t list
  -> OPTIONS.t
  -> (DATA.property list DATA.cell list) process_result Lwt.t

(** Starts a CBMC process that reads the file and return the list of cover
    objectives (i.e., instructions to cover) *)
val cbmc_get_cover_objectives
  : store: Sc_store.t
  -> runner_options: Types.runner_options
  -> entrypoint:string
  -> files:[`C] Sc_sys.File.t list
  -> OPTIONS.t
  -> (DATA.property list DATA.cell list) process_result Lwt.t

(** Starts a CBMC process that reads the file and return the list of labels *)
val cbmc_get_clabels
  : store: Sc_store.t
  -> lbls: Sc_C.Cov_label.simple list
  -> runner_options: Types.runner_options
  -> entrypoint: string
  -> files:[`C] Sc_sys.File.t list
  -> OPTIONS.t
  -> (DATA.property list DATA.cell list) process_result Lwt.t

(** Starts a CBMC process that analyzes the file and returns the test cases
    covering the labels. *)
val cbmc_cover_analysis
  : store: Sc_store.t
  -> runner_options: Types.runner_options
  -> entrypoint: string
  -> files:[`C] Sc_sys.File.t list
  -> to_cover: [`simple] analysis_env
  -> OPTIONS.t
  -> (DATA.cbmc_cover_output DATA.cell list) process_result Lwt.t

(** Starts a CBMC process that checks if the labels are uncoverable by asserting
    the negation of their condition. *)
val cbmc_assert_analysis
  : store: Sc_store.t
  -> runner_options: Types.runner_options
  -> entrypoint: string
  -> files:[`C] Sc_sys.File.t list
  -> to_cover: [`simple] analysis_env
  -> OPTIONS.t
  -> (DATA.cbmc_assert_output DATA.cell list) process_result Lwt.t

(** Same as [cbmc_assert_analysis], but uses the error labels feature of CBMC
    instead of assertions. *)
val cbmc_clabel_analysis
  : store: Sc_store.t
  -> runner_options: Types.runner_options
  -> entrypoint:string
  -> files:[`C] Sc_sys.File.t list
  -> to_cover: [`simple] analysis_env
  -> OPTIONS.t
  -> (DATA.cbmc_assert_output DATA.cell list) process_result Lwt.t
