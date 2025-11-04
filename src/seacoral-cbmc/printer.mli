(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Printers for the different types of the tool. *)

open Types
open Basics.PPrt

val pp_mode: OPTIONS.mode pp

val pp_options: OPTIONS.t pp

val pp_json_options : DATA.json_options pp

val pp_source_loc : DATA.source_location pp

val pp_message : DATA.message pp

val pp_cell : pp_data:('a pp) -> 'a DATA.cell pp

val pp_property : DATA.property pp

val pp_value : DATA.value pp

val pp_test : DATA.test pp

val pp_goal : DATA.goal pp

val pp_goals_details : DATA.goals_details pp

val pp_cbmc_cover_output : DATA.cbmc_cover_output pp

val pp_instruction : DATA.instruction pp

val pp_trace : DATA.instruction list pp

val pp_cbmc_assert_output : DATA.cbmc_assert_output pp

val pp_simple_label_env : simple_label_env pp

val pp_analysis_env : _ analysis_env pp
