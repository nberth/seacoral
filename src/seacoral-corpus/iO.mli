(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025-2026 OCamlPro                                      *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Can be used with "%a" in {!Lwt_io.fprintf} format strings *)
val print_summary : unit -> Types.test_outcome -> string
  
val scan_summary : Scanf.Scanning.in_channel -> Types.test_outcome 
