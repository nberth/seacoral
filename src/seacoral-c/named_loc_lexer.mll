(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

{ open Named_loc_parser }

let cid =
  ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]*

let num = [ '0'-'9' ]+

rule token = parse
  | [' ' '\t'] { token lexbuf }	(* Skip blanks *)
  | eof | "\n" { EOL }          (* End of the parsing *)

  (* Expression separator *)
  | "->" { ARROW }
  | "." { DOT }
  | "{struct " (cid as id) "}" { STRUCT id }
  | ":" { COLON }
  | "[_]" { BRACKETED_UNDERSCORE }

  (* When no other word has been found *)
  | cid as id { ID id }
