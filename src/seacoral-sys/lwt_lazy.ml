(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Syntax

module Syntax = struct

  let ( &&* ) b b' =
    let* b = b in
    if b then Lazy.force b'
    else Lwt.return false

  let ( ||* ) b b' =
    let* b = b in
    if b then Lwt.return true
    else Lazy.force b'

end

module type HASHTBL = sig
  type key
  type !_ t
  val create: int -> _ t
  val add: 'a t -> key -> 'a -> unit
  val find: 'a t -> key -> 'a
end

type ('a, 'b) controlled_computation = ident: 'a -> (unit -> 'b Lwt.t) -> 'b Lwt.t

let do_once
    ?(keep_idents = false)
    (type k) (module K: Hashtbl.HashedType with type t = k) =
  let module H
    = (val (if keep_idents
            then (module Hashtbl.Make (K))
            else (module Ephemeron.K1.Make (K))): HASHTBL with type key = k)
  in
  let idents_done = H.create 1 in
  let mutex = Lwt_mutex.create () in
  fun ~(ident: k) f ->
    let* mbox =
      Lwt_mutex.with_lock mutex begin fun () ->
        Lwt.return @@
        try
          H.find idents_done ident
        with Not_found ->
          let mbox = Lwt_mvar.create None in
          H.add idents_done ident mbox;
          mbox
      end
    in
    let* prev_res = Lwt_mvar.take mbox in
    let* res = match prev_res with
      | Some res -> Lwt.return res
      | None -> f ()
    in
    let* () = Lwt_mvar.put mbox (Some res) in
    Lwt.return res

let ez_do_once ~ident f =
  do_once (module struct include String let hash = Hashtbl.hash end) f
    ~keep_idents: true
    ~ident

(* --- *)

let persist_in ~dir ?(force = false) ?(donefile = ".done") doit skip =
  let open Syntax in
  let open File.Syntax in
  let done_file = dir / donefile in
  let* proceed = Lwt.return force ||* lazy (Lwt_file.misses done_file) in
  if proceed
  then begin
    let* res = doit () in
    let* () = Lwt_file.touch done_file in
    Lwt.return res
  end else begin
    skip ()
  end
