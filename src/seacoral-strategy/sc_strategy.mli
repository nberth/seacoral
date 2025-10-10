(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

module Types : module type of Types

module Printer : module type of Printer

(** Returns a normalized representation of a strategy. *)
val normalize : Types.t -> Types.t

(** [read str]

    Reads a strategy from a string expression [str]. Parsing rules are defined
    in parser.mly and lexer.mll as follows:

    - a simple readable string corresponds to a tool;
    - exp ~> exp is a sequence of strategy (~> is right associative)
    - exp // exp is the parallel application of two strategies (// is right
      associative)
    - ( exp ) is a strategy.

    For example, the strategy "cbmc ~> libfuzzer // klee ~> luncov" will
    correspond to the application of cbmc followed by the parallel application
    of libfuzzer and, in parallel, the sequence klee + luncov (meaning luncov may
    start while libfuzzer is still working.
    If you want luncov to be started after libfuzzer and klee have worked, you
    can use "cbmc ~> (libfuzzer // klee) ~> luncov"

*)
val read : string -> Types.t

(** Returns the list of strings in a given strategy *)
val tools : Types.t -> string list
