(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Workspace-handling utilities *)

open Types

open Sc_sys.File.Syntax

let mksub workspace name =
  let workdir = workspace.workdir / name in
  Sc_sys.File.touch_dir workdir;
  { workspace with workdir }

let install_resources_in ~workspace resource_installer =
  Resource.install resource_installer workspace.resroot

module LWT = struct

  open Lwt.Syntax

  let mksub workspace name =
    let workdir = workspace.workdir / name in
    let* () = Sc_sys.Lwt_file.touch_dir workdir in
    Lwt.return { workspace with workdir }

end
