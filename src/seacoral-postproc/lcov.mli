(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** Report generation with GCOV/LCOV *)

type options
val config_section: options Sc_config.Section.section

val run
  : project: _ Sc_project.Types.project
  -> Sc_project.Types.testsuite
  -> Sc_sys.File.dir option Lwt.t
