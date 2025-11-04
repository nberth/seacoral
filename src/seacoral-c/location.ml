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

type t = location

let compare i j =
  let l = i.loc_line - j.loc_line in
  if l = 0 then String.compare i.loc_file j.loc_file
  else l

(** [print fmt loc] prints a location in `filename:line` format *)
let print fmt { loc_line; loc_file } =
  Format.fprintf fmt "%s:%i" loc_file loc_line

let of_string str =
  match String.split_on_char ':' str with
  | [] ->
      Fmt.invalid_arg "%s.%s %S" __MODULE__ "of_string" str
  | [loc_file] ->
      { loc_file; loc_line = 0 }
  | loc_file :: loc_line :: _ ->
      { loc_file; loc_line = int_of_string loc_line }

module MAP = Basics.MakeMap (struct
    type t = location
    let compare = compare
    let print = print
  end)
