(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** Seacoral CLI entrypoint. *)
val main
  : ?enable_console_timing:bool
  -> ?enable_detailed_stats:bool
  -> ?enable_logfile:bool
  -> ?argv:string array
  -> unit
  -> Cmdliner.Cmd.Exit.code
