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

let parse_string entry str =
  let lexbuf = Lexing.from_string str in
  entry Ptr_specs_lexer.token lexbuf

let pointer_ref_of_string: string -> pointer_ref =
  parse_string Ptr_specs_parser.pointer_ref_main

let pointer_constraint_of_string: string -> pointer_constraint =
  parse_string Ptr_specs_parser.pointer_constraint_main

(* --- *)

let var_mem ~pointer_var (nll : pointer_refs) =
  List.mem (Variable { pointer_var }) nll

let field_mem ~struct_name ~pointer_field_name l =
  List.mem (Struct_field { struct_name; pointer_field_name }) l

let find_var ~pointer_var:pv (nlal : pointer_constraints) : string option =
  List.find_map begin function
    | Distinct_variables { pointer_var; size_var } when pointer_var = pv ->
        Some size_var
    | Distinct_variables _
    | From_same_struct _ ->
        None
  end nlal

let find_field ~struct_name:sn ~pointer_field_name:pn =
  List.find_map begin function
    | From_same_struct { struct_name; pointer_field_name; size_field_name }
      when struct_name = sn && pointer_field_name = pn ->
        Some size_field_name
    | From_same_struct _
    | Distinct_variables _ ->
        None
  end
