(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025-2026 OCamlPro                                      *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

%{ open Types %}

%token EOL

%token LEFT_BRACKET "{"
%token RIGHT_BRACKET "}"
%token STRUCT_KWD "struct"
%token DOT "."
%token COLON ":"
%token<string> ID

%start pointer_ref_main
%start pointer_constraint_main

%type <pointer_ref> pointer_ref_main
%type <pointer_constraint> pointer_constraint_main

(* Grammar follows *)
%%

pointer_ref_main: pointer_ref EOL { $1 }
pointer_constraint_main: pointer_constraint EOL { $1 }

struct_:
  | LEFT_BRACKET STRUCT_KWD ID RIGHT_BRACKET { $3 }
  ;

pointer_ref:
  | ID {
      Variable { pointer_var = $1 }
    }
  | struct_ DOT ID {
      Struct_field { struct_name = $1; pointer_field_name = $3 }
    }
  ;

(* TODO: make a simpler syntax for structures *)
pointer_constraint:
  | struct_ COLON ID COLON ID {
      From_same_struct {
          struct_name = $1;
          pointer_field_name = $3;
          size_field_name = $5;
        }
    }
  | ID COLON ID {
      Distinct_variables {
          pointer_var = $1;
          size_var = $3;
      }
    }
  ;
