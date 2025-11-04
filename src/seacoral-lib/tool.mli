(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Tool manager. Allows to register analyzers (Klee, libfuzzer, CBMC, ...) *)

(* --- *)

type tool =
  {
    tool_setup: (unit -> unit Lwt.t) Tool_sigs.setup;
    tool_config: Sc_config.Section.any_section option;
    tool_labels: targeted_labels;
    tool_availability_check: unit -> bool Lwt.t;
    tool_kind: [`Preprocess | `Static | `Dynamic];
  }

and targeted_labels =
  | For_simple_labels
  | For_any_label

val register_module
  : targeted_labels
  -> (module Tool_sigs.WITH_PARAMETRIC_PROJECT)
  -> name:string
  -> kind:[ `Dynamic | `Preprocess | `Static ]
  -> config_section: _ Sc_config.Section.section option
  -> availability_check:(unit -> bool Lwt.t)
  -> unit

val register_module_with_boxed_project
  : targeted_labels
  -> (module Tool_sigs.WITH_BOXED_PROJECT)
  -> name:string
  -> kind:[ `Dynamic | `Preprocess | `Static ]
  -> config_section: _ Sc_config.Section.section option
  -> availability_check:(unit -> bool Lwt.t)
  -> unit

val register_functions
  : targeted_labels
  -> setup:'b Tool_sigs.setup
  -> run:('b -> unit Lwt.t)
  -> name:string
  -> kind:[ `Dynamic | `Preprocess | `Static ]
  -> config_section: _ Sc_config.Section.section option
  -> availability_check:(unit -> bool Lwt.t)
  -> unit

val register_function
  : targeted_labels
  -> setup:(unit -> unit Lwt.t) Tool_sigs.setup
  -> name:string
  -> kind:[ `Dynamic | `Preprocess | `Static ]
  -> config_section: _ Sc_config.Section.section option
  -> availability_check:(unit -> bool Lwt.t)
  -> unit

val find: string -> tool

val is_known: string -> bool

val known_tool_names: unit -> string list
val known_config_sections: unit -> Sc_config.Section.any_section list
