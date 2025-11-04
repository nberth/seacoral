(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

{ open Parser }

let tool_name =
  ['a'-'z' 'A'-'Z' '0'-'9' '+' '-' '_' ]+

let any_word = _+

rule token = parse
  | [' ' '\t'] { token lexbuf }	(* Skip blanks *)
  | eof | "\n" { EOL }          (* End of the parsing *)

  (* Expression separator *)
  | "~>" { SEQ }
  | "//" { PAR }

  (* Symbols *)
  | "(" { LP }
  | ")" { RP }

  (* When no other word has been found *)
  | tool_name as t { TOOL t }
