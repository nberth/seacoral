/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU Affero General    */
/*  Public License.                                                       */
/*                                                                        */
/**************************************************************************/

%{
    open Types

    let sequence s1 s2 =
    	match s1, s2 with
	| Sequence l1, Sequence l2 -> Sequence (NEL.append l1 l2)
	| Sequence l, _ -> Sequence NEL.(append l (One s2))
	| _, Sequence l -> Sequence (s1 :: l)
	| _ -> Sequence (s1 :: One s2)

    let parallel s1 s2 =
    	match s1, s2 with
	| Parallel l1, Parallel l2 -> Parallel (NEL.append l1 l2)
	| Parallel l, _ -> Parallel NEL.(append l (One s2))
	| _, Parallel l -> Parallel (s1 :: l)
	| _ -> Parallel (s1 :: One s2)
%}

%token EOL

/* Strategy separator */
%token SEQ
%token PAR
%token<string> TOOL

/* Parentheses */
%token LP
%token RP

/* */
%right SEQ PAR

%start main

%type <Types.t> main
%type <Types.t> exp

/* Grammar follows */
%%

main:	exp EOL			{ $1 }
;

/* Parentheses */

exp:
  | LP exp RP { $2 }
  | exp SEQ exp { sequence $1 $3 }
  | exp PAR exp { parallel $1 $3 }
  | TOOL { Tool $1 }
