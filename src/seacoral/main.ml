(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

let () =
  Printexc.record_backtrace true;
  let enable_console_timing =
    match Sys.getenv_opt "SC_ENABLE_CONSOLE_TIMING" with
    | Some ("no" | "0") -> false
    | _ -> true
  and enable_detailed_stats =
    match Sys.getenv_opt "SC_ENABLE_DETAILED_STATS" with
    | Some ("no" | "0") -> false
    | _ -> true
  and enable_display_outcomes =
    match Sys.getenv_opt "SC_ENABLE_DISPLAY_OUTCOMES" with
    | None
    | Some ("no" | "0") -> None
    | Some ("v" | "verbose") -> Some `Verbose
    | Some _ -> Some `Yes
  in
  exit (Sc.Main.main ~enable_console_timing ~enable_detailed_stats ?enable_display_outcomes ())
