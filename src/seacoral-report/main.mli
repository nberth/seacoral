(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** From a static database, writes the HTML report of the analysis. *)
val generate_report
  : _ Sc_project.Types.project
  -> Sc_project.Types.testsuite
  -> [`html] Sc_sys.File.t option Lwt.t
