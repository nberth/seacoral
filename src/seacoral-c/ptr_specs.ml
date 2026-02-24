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

let parse_string ~parsed_item ptr_specs_entry str =
  let lexbuf = Lexing.from_string str in
  try
    Ok (ptr_specs_entry Ptr_specs_lexer.token lexbuf)
  with Ptr_specs_parser.Error ->
    Error (Syntax_error { expected = parsed_item; string = str })

let pointer_ref_of_string: string -> (pointer_ref, error) result =
  parse_string Ptr_specs_parser.pointer_ref_main
    ~parsed_item:Pointer_reference

let pointer_constraint_of_string: string -> (pointer_constraint, error) result =
  parse_string Ptr_specs_parser.pointer_constraint_main
    ~parsed_item:Pointer_constraint

(* --- *)

let var_mem ~pointer_var (nll : pointer_refs) =
  List.mem (Variable { pointer_var }) nll

let field_mem ~struct_name ~pointer_field_name l =
  List.mem (Struct_field { struct_name; pointer_field_name }) l

let find_size_var ~pointer_var:pv (nlal : pointer_constraints) : string option =
  List.find_map begin function
    | Distinct_variables { pointer_var; size_var } when pointer_var = pv ->
        Some size_var
    | Distinct_variables _
    | From_same_struct _ ->
        None
  end nlal

let find_size_field ~struct_name:sn ~pointer_field_name:pn =
  List.find_map begin function
    | From_same_struct { struct_name; pointer_field_name; size_field_name }
      when struct_name = sn && pointer_field_name = pn ->
        Some size_field_name
    | From_same_struct _
    | Distinct_variables _ ->
        None
  end
