(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Infix
open Lwt.Syntax

let conds = Hashtbl.create 1
and lock = Lwt_mutex.create ()
let ez_processwide ~(ident: string) doit =
  let rec tryit () =
    match Hashtbl.find_opt conds ident with
    | None ->
        let cond = Lwt_condition.create () in
        Hashtbl.add conds ident cond;
        let* res = doit () in
        Lwt_condition.broadcast cond ();
        Hashtbl.remove conds ident;
        Lwt.return res
    | Some cond ->
        Lwt_condition.wait ~mutex:lock cond >>= tryit
  in
  Lwt_mutex.with_lock lock tryit

let global_lock = Lwt_mutex.create ()
let ez_global f = Lwt_mutex.with_lock global_lock f
