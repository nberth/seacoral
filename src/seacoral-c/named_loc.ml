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

let of_varname ?(access_path = []) s = {prefix = Variable s; access_path}

let if_varname nl =
  match nl.prefix with
  | Variable v when nl.access_path = [] -> Some v
  | _ -> None

let of_string (str: string) : named_location =
  let lexbuf = Lexing.from_string str in
  Named_loc_parser.named_loc_main Named_loc_lexer.token lexbuf

let assoc_of_string (str: string) : named_loc_assoc =
  let lexbuf = Lexing.from_string str in
  Named_loc_parser.named_loc_assoc_main Named_loc_lexer.token lexbuf

let var_mem (varname : string) (nll : named_location list) =
  List.mem (of_varname varname) nll

let find_assoc
    ~varname
    ~access_path
    (nlal : named_loc_assoc list) : (string * abstract_access_path) option =
  List.find_map (function
      | Separate_variables {array; size} ->
          (* If they are separate variables, we can just check if the array is
             the same. *)
          if array = {prefix = Variable varname; access_path} then
            Some size
          else
            None
      | From_same_struct {struct_name = _; array; size} ->
          if access_path <> array then
            (* The types or accesses are different. *)
            None
          else
            Some (varname, size)
    )
    nlal

let find_assoc' nl nlal =
  match nl.prefix with
  | Variable v ->
      begin
        match find_assoc ~varname:v ~access_path:nl.access_path nlal with
        | Some (v, access_path) -> Some {prefix = Variable v; access_path}
        | None -> None
      end
  | Struct _ ->
      begin
        match find_assoc ~varname:"" ~access_path:nl.access_path nlal with
        | Some (_, access_path) -> Some {nl with access_path}
        | None -> None
      end
