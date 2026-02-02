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

let of_varname ?(access_path = []) s =
  { prefix = Variable s; access_path }

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

let field_mem ~struct_name ~field_name l =
  List.mem { prefix = Struct struct_name;
             access_path = [Access_field field_name] } l

let find_var ~varname (nlal : named_loc_assoc list) : string option =
  List.find_map begin function
    | Distinct_variables { pointer_var; size_var } when pointer_var = varname ->
        Some size_var
    | Distinct_variables _
    | From_same_struct _ ->
        None
  end nlal

let find_field ~struct_name:sn ~field_name =
  List.find_map begin function
    | From_same_struct { struct_name; pointer_field_name; size_field_name }
      when struct_name = sn && pointer_field_name = field_name ->
        Some size_field_name
    | From_same_struct _
    | Distinct_variables _ ->
        None
  end
