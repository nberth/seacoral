(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** Harness generation *)

module TYPES: sig

  type options =
    {
      symbolization_scheme: [`full_struct | `independent_fields];
    }

  type support_data

  type printer =
    {
      pp_preamble: Basics.PPrt.pu;
      pp_body: Basics.PPrt.pu;
    }

  type runtime_params =
    {
      symsize_capacity: int;
    }

end

include module type of TYPES
  with type options = TYPES.options
   and type printer = TYPES.printer
   and type runtime_params = TYPES.runtime_params

val support_data: options -> _ Sc_project.Types.project_params -> support_data
val entrypoint: support_data -> string
val runtime_params: support_data -> runtime_params
val printer: support_data -> printer
