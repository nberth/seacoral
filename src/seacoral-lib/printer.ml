(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Types

let pp_config_error ppf = function
  | Missing_input_file ->
      Fmt.pf ppf "missing@ input@ file(s)"
  | Missing_entrypoint ->
      Fmt.pf ppf "missing@ entrypoint"
  | Missing_tools ->
      Fmt.pf ppf "missing@ tools"
  | Unknown_tools One tool ->
      Fmt.pf ppf "unknown@ tool@ %s" tool
  | Unknown_tools tools ->
      Fmt.pf ppf "unknown@ tools@ %a"
        Basics.PPrt.(with_oxford_comma Fmt.string) (NEL.to_list tools)

let pp_exn_lines ppf exn =
  Fmt.lines ppf (Printexc.to_string exn)

let pp_backtrace_lines ppf backtrace =
  Fmt.lines ppf (Printexc.raw_backtrace_to_string backtrace)

let pp_exn_n_backtrace_lines ppf (exn, backtrace) =
  Fmt.pf ppf "@[<v>%a@;%a@]" pp_exn_lines exn pp_backtrace_lines backtrace

let pp_tool_preprocessing_error ppf = function
  | Failed_preprocessing { tool; error } ->
      Sc_core.Printer.pp_generic_preprocessing_error ppf error
        ~pp_operation:(fun ppf -> Fmt.fmt "Preprocessing@ by@ %s" ppf tool)
        ~pp_error:pp_exn_n_backtrace_lines

let pp_tool_computation_error ppf = function
  | Failed_computation { tool; error; backtrace } ->
      Fmt.pf ppf "Computation@ by@ %s@ failed:@;%a\
                 " tool pp_exn_n_backtrace_lines (error, backtrace)

let pp_generation_error ppf = function
  | Project_setup_error e ->
      Sc_project.Printer.pp_setup_error ppf e
  | Project_labeling_error e ->
      Sc_project.Printer.pp_labeling_error ppf e
  | Project_elaboration_error e ->
      Sc_project.Printer.pp_elaboration_error ppf e
  | Tool_preprocessing_error e ->
      pp_tool_preprocessing_error ppf e
  | Tool_computation_error e ->
      pp_tool_computation_error ppf e
