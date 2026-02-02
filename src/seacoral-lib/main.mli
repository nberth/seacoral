(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

type generation_options =
  {
    run: Types.run_config;
    enable_detailed_stats: bool;
    strategy: Sc_strategy.Types.t;
    print_statistics: bool;
  }

(** Main entrypoint for the library: actually performs testcase generation. *)
val generate
  : project_config: Sc_project.Types.project_config
  -> encoding_params: Sc_values.TYPES.encoding_params
  -> generation_options
  -> unit Lwt.t
