(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** Pretty-printer for a configuration error *)
val pp_config_error: Types.config_error Fmt.t

(** Pretty-printer for asynchronous errors that happen during tool
    preprocessing *)
val pp_tool_preprocessing_error: Types.tool_preprocessing_error Fmt.t

(** Pretty-printer for asynchronous errors that happen during tool execution *)
val pp_tool_computation_error: Types.tool_computation_error Fmt.t

(** Pretty-printer for asynchronous errors that happen during generation *)
val pp_generation_error: Types.generation_error Fmt.t
