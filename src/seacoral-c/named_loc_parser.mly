/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU General Public    */
/*  License version 3.                                                    */
/*                                                                        */
/**************************************************************************/

%{ open Types %}

%token EOL

/* Identifier combinator */
%token ARROW
%token DOT
%token<string> STRUCT
%token<string> ID
%token COLON
%token BRACKETED_UNDERSCORE


/* */

%start named_loc_main
%start named_loc_assoc_main

%type <named_location> named_loc_main
%type <named_location> named_loc
%type <named_loc_assoc> named_loc_assoc_main
%type <named_loc_assoc> named_loc_assoc


/* Grammar follows */
%%

named_loc_main: named_loc EOL { $1 }
named_loc_assoc_main: named_loc_assoc EOL { $1 }
;

nl_prefix:
  | STRUCT { Struct $1 }
  | ID { Variable $1 }

access_path_node:
  | BRACKETED_UNDERSCORE { [Access_ptr] }
  | DOT ID { [Access_field $2] }
  | ARROW ID { [Access_ptr; Access_field $2] }

access_path:
  | { [] }
  | access_path access_path_node { $1 @ $2 }

named_loc:
  | nl_prefix access_path { {prefix = $1; access_path = $2} }

/* TODO: make a simpler syntax for structures */
named_loc_assoc:
  | STRUCT access_path COLON access_path {
      From_same_struct {struct_name = $1; array = $2; size = $4}
    }
  | STRUCT access_path COLON ID access_path {
      Separate_variables {
        array = {prefix = Struct $1; access_path = $2}; size = ($4, $5)
      }
    }
  | ID access_path COLON ID access_path {
      Separate_variables {
        array = {prefix = Variable $1; access_path = $2}; size = ($4, $5)
      }
    }
