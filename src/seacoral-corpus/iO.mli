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
val print_sanitizer_error_summary
  : unit -> Types.sanitizer_error_summary -> string

val scan_sanitizer_error_summary
  : Scanf.Scanning.in_channel -> Types.sanitizer_error_summary
