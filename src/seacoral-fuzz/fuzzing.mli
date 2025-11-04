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

val install_resources_in
  : workspace: Sc_core.Types.workspace
  -> dir Lwt.t

(** Todo: document this function. *)
val make_harness_cxx
  : (module Ez_logs.T)
  -> outdir: dir
  -> ?dry: bool
  -> _ Sc_project.Types.project
  -> [`CXX] file Lwt.t
