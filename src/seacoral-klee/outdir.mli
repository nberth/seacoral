(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** [fresh ~workspace ~tested_fun] returns a non-existing working directory in
    [workspace], to operate on funciton [tested_fun]. *)
val fresh
  : workspace: Sc_core.Types.workspace
  -> tested_fun: string
  -> Sc_sys.File.dir Lwt.t
