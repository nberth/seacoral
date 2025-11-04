(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

val ez_processwide: ident:string -> (unit -> 'b Lwt.t) -> 'b Lwt.t
val ez_global: (unit -> 'a Lwt.t) -> 'a Lwt.t
