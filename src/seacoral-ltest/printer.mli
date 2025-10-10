(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** [pp_lreplay_results] pretty-prints results of lreplay in a classical way,
    with first a summary of coverage, and then one label status result per
    line. *)
val pp_lreplay_results: (Types.lreplay_results *
                         Types.lreplay_display_style) Fmt.t

val pp_error: Types.error Fmt.t
