(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

{ open Ptr_specs_parser }

let cid =
  ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]*

rule token = parse
  | [' ' '\t'] { token lexbuf }	(* Skip blanks *)
  | eof | "\n" { EOL }          (* End of the parsing *)

  (* Expression separator *)
  | "{" { LEFT_BRACKET }
  | "}" { RIGHT_BRACKET }
  | "struct" { STRUCT_KWD }
  | "." { DOT }
  | ":" { COLON }

  (* When no other word has been found *)
  | cid as id { ID id }
