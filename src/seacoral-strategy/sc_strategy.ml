(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Types

module Types = Types
module Printer = Printer

let rec normalize = function
  | Nothing | Tool _ as t -> t
  | Parallel l -> normalize_list l (fun res -> Parallel res)
  | Sequence l -> normalize_list l (fun res -> Sequence res)

and normalize_list l cont =
  match
    NEL.filter_map ~f:(fun t -> match normalize t with Nothing -> None | s -> Some s) l
  with
  | None -> Nothing
  | Some One t -> t
  | Some l -> cont l

let read str =
  let lexbuf = Lexing.from_string (str ^ "\n") in
  try Parser.main Lexer.token lexbuf with
  | Failure s -> raise (Parsing_failure s)

let tools s =
  let rec aux acc = function
    | Nothing -> acc
    | Tool s -> Basics.Strings.add s acc
    | Parallel l
    | Sequence l -> NEL.fold_left ~f:aux acc l
  in
  let set = aux Basics.Strings.empty s in
  Basics.Strings.elements set
