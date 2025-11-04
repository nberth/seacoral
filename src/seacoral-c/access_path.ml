(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Operations on access paths *)

open Types

type t = access_path

type suffix = access_path_suffix

(* Printing *)

let print_ap_operation ppf = function
  | Field_access f -> Fmt.pf ppf ".%s" f
  | Index_access i -> Fmt.pf ppf "[%i]" i

let print_suffix ppf =
  NEL.pp ~fsep:"" ~fopen:"" ~fclose:"" print_ap_operation ppf

let print ppf = function
  | Access_path_origin o ->
      Fmt.string ppf o
  | Access_path_suffix s ->
      print_suffix ppf s
  | Access_path { origin; suffix } ->
      Fmt.pf ppf "%s%a" origin print_suffix suffix

let to_string i = Fmt.str "%a" print i

(* Construction *)

let field: string -> suffix = fun f ->
  One (Field_access f)

let index: int -> suffix = fun i ->
  One (Index_access i)

let from: string -> suffix option -> t = fun origin -> function
  | Some suffix -> Access_path { origin; suffix }
  | None -> Access_path_origin origin

let append_to_suffix: suffix -> suffix -> suffix =
  NEL.append

let append_to_suffix': suffix option -> suffix -> suffix = function
  | None -> Fun.id
  | Some ap -> append_to_suffix ap

let origin_only: string -> t = fun o ->
  from o None

let suffix_only: suffix -> t = fun s ->
  Access_path_suffix s

let append: t -> suffix -> t = fun ap s' ->
  match ap with
  | Access_path_origin origin ->
      Access_path { origin; suffix = s' }
  | Access_path_suffix s ->
      Access_path_suffix (NEL.append s s')
  | Access_path { origin; suffix } ->
      Access_path { origin; suffix = NEL.append suffix s' }

let append_field t f = append t (field f)

let append_index t i = append t (index i)

(* Miscellaneous *)

let origin' = function
  | Access_path {origin; _}
  | Access_path_origin origin -> origin
  | Access_path_suffix _ -> invalid_arg "Access_path.origin'"

let origin t =
  try Some (origin' t) with Invalid_argument _ -> None

let suffix' = function
  | Access_path {suffix; _}
  | Access_path_suffix suffix -> suffix
  | Access_path_origin _ -> invalid_arg "Access_path.suffix'"

let suffix t =
  try Some (suffix' t) with Invalid_argument _ -> None

let compare_path_op i j =
  match i, j with
  | Field_access s, Field_access s' -> String.compare s s'
  | Index_access i, Index_access i' -> Int.compare i i'
  | Field_access _, Index_access _ -> 1
  | Index_access _, Field_access _ -> -1

let compare_suffix i j = NEL.compare compare_path_op i j

let compare i j =
  match i, j with
  | Access_path_origin i, Access_path_origin j -> String.compare i j
  | Access_path_suffix i, Access_path_suffix j -> compare_suffix i j
  | Access_path {origin = o; suffix = s},
    Access_path {origin = o'; suffix = s'} ->
     let c = String.compare o o' in
     if c = 0 then compare_suffix s s' else c
  | Access_path_origin _, (Access_path_suffix _ | Access_path _)
  | Access_path_suffix _, Access_path _ -> 1
  | Access_path _, (Access_path_origin _ | Access_path_suffix _)
  | Access_path_suffix _, Access_path_origin _ -> -1

(* Query *)

(* let suffix: t -> suffix option = function *)
(*   | Access_path_origin _ *)
(*   | Access_path_suffix One _ -> None *)
(*   | Access_path_suffix (_ :: suffix) *)
(*   | Access_path { suffix; _ } -> Some suffix *)

(** Various (hackish / brittle) stuff *)
module HACK = struct

  let forget_first_suffix_punct: suffix -> t = function
    | One Field_access field ->
        origin_only field
    | Field_access field :: suffix ->
        from field (Some suffix)
    | One Index_access _
    | Index_access _ :: _ ->
        Fmt.invalid_arg "%s.%s" __MODULE__ "forget_first_punct"

  let shift_left: t -> t = function
    | Access_path_suffix (_ :: One Field_access origin)
    | Access_path { suffix = One Field_access origin; _ } ->
        Access_path_origin origin
    | Access_path_suffix (_ :: Field_access origin :: suffix)
    | Access_path { suffix = Field_access origin :: suffix; _ } ->
        Access_path { origin; suffix }
    | Access_path_origin _
    | Access_path_suffix _
    | Access_path _ ->
        Fmt.invalid_arg "%s.%s" __MODULE__ "shift_left"

end

module C = struct type nonrec t = t let compare = compare end
module Set = Set.Make(C)
module Map = Map.Make(C)
