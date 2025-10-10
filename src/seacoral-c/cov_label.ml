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

type 'k t = 'k cov_label
type simple = [`simple] t
type hyper  = [`hyper]  t
type nonrec any = any t

let inspect: type k. k t -> _ = function
  | S l -> S l | Any S l -> S l
  | H l -> H l | Any H l -> H l

let as_any (type k) : k t -> Types.any t = function
  | (S _ | H _) as l -> Any l
  | Any _ as l -> l

let rec view: type k. k t -> cov_label_view = function
  | S l | H l -> l
  | Any l -> view l

let id       l = (view l).cov_label_id
let status   l = (view l).cov_label_status
let emitter  l = (view l).cov_label_emitter
let location l = (view l).cov_label_loc

let is_covered     l = match status l with Covered _ -> true | _ -> false
let is_uncoverable l = status l = Uncoverable
let is_unknown     l = status l = Unknown

let max_label_id: _ t list -> int = fun lbls ->
  let exception Overlapping of int in
  let exception IdTooHigh of int in
  try
    let len = List.length lbls in
    let arr = Array.make len None in    (* Could be replaced by (Obj.magic 0) *)
    List.iter
      (fun l ->
         let id = id l in
         match arr.(id - 1) with
         | None -> arr.(id - 1) <- Some l
         | Some _ -> raise @@ Overlapping id
         | exception Invalid_argument _ -> raise @@ IdTooHigh id) lbls;
    len
  with
  | Overlapping i ->
      Fmt.invalid_arg "sort_labels (overlapping %i)" i
  | IdTooHigh i ->
      Fmt.invalid_arg "sort_labels (id %i too high)" i

(** {2 Status} *)

let show_status = function
  | Unknown -> "Unknown"
  | Uncoverable -> "Uncoverable"
  | Covered _ -> "Covered"

let set_view_status s v = {v with cov_label_status = s}

let set_status (type a) s (lbl : a t) : a t =
  match lbl with
  | S l -> S (set_view_status s l)
  | H l -> H (set_view_status s l)
  | Any (S l) -> Any (S (set_view_status s l))
  | Any (H l) -> Any (H (set_view_status s l))

let print_status ppf s =
  Fmt.string ppf (show_status s)
