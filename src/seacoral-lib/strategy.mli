(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** Creating and running orchestration strategies *)

(** Checks the validity of a tools specification list.

    Raises {!Types.CONFIG_ERROR} if [tool_list] is empty, or if no listed tool
    is known. *)
val check_tools_spec: string list -> unit

(** [make tools_spec config] creates a strategy from a list of tool names
    [tools_spec] (or [["*"]] to select every known tool), and a strategy
    configuration [config].

    Raises {!Types.CONFIG_ERROR} if [tool_list] is empty, or if no listed tool
    is known. *)
val make
  : string list
  -> Types.orchestration_strategy
  -> Sc_strategy.Types.t

(** Play a strategy on a given project *)
val play
  : project: 'raw_test Sc_project.Types.project
  -> options: Sc_core.Types.initialization_options
  -> Sc_ltest.Types.labels
  -> Sc_strategy.Types.t
  -> unit Lwt.t
