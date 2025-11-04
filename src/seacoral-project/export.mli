(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Options guiding export of test cases in C *)

type options
val config_section: options Sc_config.Section.section

val write_testsuite
  : ?exclude: Basics.Digests.t
  -> _ Types.project
  -> Types.testsuite Lwt.t

val assume_testsuite
  : _ Types.project
  -> Types.testsuite Lwt.t

val oracle_verdict_assessment_preproc_flag: string
val sufficient_generated_headers: _ Types.project -> [`h] Sc_sys.File.t list
val sufficient_cppflags: _ Types.project -> string list
