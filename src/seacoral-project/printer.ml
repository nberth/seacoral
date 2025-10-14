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

let pp_entrypoint_name ppf project =
  Fmt.string ppf project.extra.given_entrypoint_name

(** Pretty prints coverage statistics. *)
let pp_coverage_info ppf ((covinfo, corpus_info): info) =
  Fmt.styled `Underline begin fun ppf () ->
    Fmt.fmt "%a@ with@ %a" ppf
      Sc_store.Printer.pp_covinfo covinfo
      Sc_corpus.Printer.pp_coverage_info corpus_info
  end ppf ()

(** Pretty prints crash statistics. *)
let pp_crash_info ppf ((_covinfo, corpus_info): info) =
  Fmt.styled `Underline begin
    if Sc_corpus.has_crashes corpus_info then
      Fmt.styled `Red begin fun ppf () ->
        Fmt.fmt "rte:@ %a" ppf
          Sc_corpus.Printer.pp_crash_info corpus_info
      end
    else
      Fmt.styled `Green @@ Fmt.any "rte:@ none"
  end ppf ()

(** Pretty prints oracle failure statistics. *)
let pp_oracle_failures_info ppf ((_covinfo, corpus_info): info) =
  Fmt.styled `Underline begin
    if Sc_corpus.has_oracle_failures corpus_info then
      Fmt.styled `Red begin fun ppf () ->
        Fmt.fmt "fails:@ %a" ppf
          Sc_corpus.Printer.pp_oracle_failures_info corpus_info
      end
    else
      Fmt.styled `Green @@ Fmt.any "fails:@ none"
  end ppf ()

let pp_setup_error =
  Sc_ltest.Printer.pp_error

let pp_exn_lines ppf exn =
  Fmt.lines ppf (Printexc.to_string exn)

let pp_labeling_error ppf = function
  | Failed_labeling error ->
      Sc_core.Printer.pp_generic_preprocessing_error ppf error
        ~pp_operation:(Fmt.fmt "Labeling") ~pp_error:pp_exn_lines
  | Syntax_errors { cmd_error; stderr_lines } ->
      ignore cmd_error;
      Fmt.pf ppf "@[<v>@[<h>Syntax@ errors@ detected@ in@ codebase:@]@;";
      if stderr_lines <> []
      then Fmt.(list (styled `Faint @@ string) ~sep:cut) ppf stderr_lines
      else Fmt.fmt "@[<h>(see@ log@ output@ above@ for@ details)@]" ppf;
      Fmt.pf ppf "@]"

let pp_elaboration_error ppf = function
  | Failed_elaboration exn ->
      Fmt.pf ppf "Elaboration@ failed:@;%a" pp_exn_lines exn
  | Unknown_function { fun_name; c_file } ->
      Fmt.pf ppf "Elaboration@ failed:@;function@ `%s'@ not@ found@ in@ %a" fun_name
        Fmt.(option ~none:(any "codebase") Sc_sys.File.print_basename) c_file
  | Unknown_formals { formals; func } ->
      Fmt.pf ppf "Elaboration@ failed:@;%a"
        Fmt.(hovbox @@ list ~sep:semi begin fun ppf formal ->
            pf ppf "function@ `%s' does@ not@ have@ any@ formal@ argument@ \
                    named@ `%s',@ nor@ does@ exist@ a@ global@ variable@ with@ \
                    this@ name" func.func_name formal
          end) (Basics.Strings.elements formals)
  | Unsupported_formals { formals; func } ->
      Fmt.pf ppf "Elaboration@ failed:@;%a"
        Fmt.(hovbox @@ list ~sep:semi begin fun ppf formal ->
            pf ppf "function@ `%s''s@ formal@ argument@ `%s'@ has@ an@ \
                    unsupported@ type" func.func_name formal
          end) (Basics.Strings.elements formals)

(* --- *)

module C = struct
  let emit_testcall ~oracle_assessment ~emit_effective_inputs
      { func_repr = entrypoint; init_func; oracle_func;
        seek_oracle_failures; _ } =
    Sc_C.Printer.emit_testcall ~entrypoint ?init_func
      ?oracle_func:(if seek_oracle_failures then oracle_func else None)
      ~oracle_assessment ~emit_effective_inputs
end

(* --- *)

;; Printexc.register_printer begin function
  | SETUP_ERROR e ->
      Some (Basics.PPrt.to_string "%a" pp_setup_error e)
  | LABELING_ERROR e ->
      Some (Basics.PPrt.to_string "%a" pp_labeling_error e)
  | ELABORATION_ERROR e ->
      Some (Basics.PPrt.to_string "%a" pp_elaboration_error e)
  | _ ->
      None
end;;
