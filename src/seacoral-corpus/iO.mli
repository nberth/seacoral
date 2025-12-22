(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Can be used with "%a" in {!Lwt_io.fprintf} format strings *)
val print_summary
  : unit
    -> [`Crash of Types.sanitizer_error_summary | `Cover of Basics.Ints.t]
    -> string
  
val scan_summary
  : Scanf.Scanning.in_channel
    -> [`Crash of Types.sanitizer_error_summary | `Cover of Basics.Ints.t]
