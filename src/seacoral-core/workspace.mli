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

(** [mksub workspace name] creates a sub-workspace [name] inside
    [workspace]. Creates the underlying working directory if it does not exist
    already. *)
val mksub: Types.workspace -> string -> Types.workspace

val install_resources_in: workspace: Types.workspace -> Resource.installer_handle
  -> Sc_sys.File.dir Lwt.t

(** LWT-ized equivalent to functions above (to be favored whenever possible in
    LWT territory). *)
module LWT: sig
  val mksub: Types.workspace -> string -> Types.workspace Lwt.t
end
