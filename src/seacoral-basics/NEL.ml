(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** Representation for non-empty lists *)

module TYPES = struct
  type 'a nel =
    | One of 'a
    | (::) of 'a * 'a nel
end
open TYPES

type 'a t = 'a nel

(** [compare_lazy cmp nel0 nel1] compares [nel0] and [nel1] using [cmp].
    [compare_lazy] is slightly more lazy than its [compare] counterpart, but the
    order is not lexicographical. *)
let compare_lazy cmp a b =
  let rec aux a b = match a, b with
    | One a, One b -> cmp a b
    | One _, _ -> -1
    | _, One _ -> 1
    | a :: a', b :: b' -> let c = cmp a b in if c = 0 then aux a' b' else c
  in
  aux a b

(** [compare cmp nel0 nel1] lexicographically orders [nel0] and [nel1] using
    [cmp]. *)
let compare cmp a b =
  let rec aux a b = match a, b with
    | One a, One b ->
        cmp a b
    | a :: _, One b  ->
        let c = cmp a b in
        if c = 0 then 1 else c
    | One a, b :: _ ->
        let c = cmp a b in
        if c = 0 then -1 else c
    | a :: a', b :: b' ->
        let c = cmp a b in
        if c = 0 then aux a' b' else c
  in
  aux a b

let equal eq a b =
  let rec aux a b = match a, b with
    | One a, One b -> eq a b
    | a :: a', b :: b' when eq a b -> aux a' b'
    | _ -> false
  in
  aux a b

let hd = function
  | One x
  | x :: _ -> x

let rec last = function
  | One x -> x
  | _ :: tl -> last tl

let fold_left ~f acc l =
  let rec aux acc = function
    | One x -> f acc x
    | x :: tl -> aux (f acc x) tl
  in
  aux acc l

let rec of_list = function
  | [] -> Fmt.invalid_arg "of_list"
  | [x] -> One x
  | [x; y] -> x :: One y
  | x :: tl -> x :: of_list tl

let to_list l =
  let rec aux acc = function
    | One x -> List.rev_append acc [x]
    | x :: tl -> aux (List.cons x acc) tl
  in
  aux [] l

let cons_list x = function
  | [] -> One x
  | l -> x :: of_list l

let rev_map f =
  let rec aux acc = function
    | One x -> f x :: acc
    | x :: tl -> aux (f x :: acc) tl
  in
  function
  | One x -> One (f x)
  | hd :: tl -> aux (One (f hd)) tl

let rev l =
  rev_map Fun.id l

let append a b =
  let rec aux b = function
    | One x -> x :: b
    | x :: tl -> x :: aux b tl
  in aux b a

let rev_to_list l =
  let rec aux acc = function
    | One x -> List.cons x acc
    | x :: tl -> aux (List.cons x acc) tl
  in
  aux [] l

let of_rev_list =
  let rec aux acc = function
    | [] -> acc
    | x :: tl -> aux (x :: acc) tl
  in
  function
  | [] -> Fmt.invalid_arg "of_rev_list"
  | last :: tl -> aux (One last) tl

let filter_map ~f l =
  let rec aux acc = function
    | One x ->
        (match f x, acc with
         | None, [] -> None
         | Some y, [] -> Some (One y)
         | None, acc -> Some (of_rev_list acc)
         | Some y, acc -> Some (of_rev_list (List.cons y acc)))
    | x :: tl ->
        (match f x with
         | None -> aux acc tl
         | Some y -> aux (List.cons y acc) tl)
  in
  aux [] l

let map ~f l =
  let rec aux acc = function
    | One x -> of_rev_list (List.cons (f x) acc)
    | x :: tl -> aux (List.cons (f x) acc) tl
  in
  aux [] l

let iter ~f l =
  let rec aux = function
    | One x -> f x
    | x :: tl -> f x; aux tl
  in
  aux l

let exists ~f l =
  let rec aux = function
    | One x -> f x
    | x :: tl -> f x || (aux[@tailcall]) tl
  in
  aux l

let pp ?fsep ?fopen ?fclose pp_e ppf list =
  Basics.PPrt.pp_lst ?fopen ?fsep ?fclose pp_e ppf (to_list list)

module INFIX = struct
  let (@) = append
end
