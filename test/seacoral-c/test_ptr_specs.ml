(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025-2026 OCamlPro                                      *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

module PS = Sc_C.Ptr_specs

let check_list parser printer =
  List.iter begin fun str ->
    match parser str with
    | Ok ps ->
        Fmt.pr "%S: @[%a@]@\n" str printer ps
    | Error e ->
        Fmt.pr "%S: @[%a@]@\n" str Sc_C.Printer.pp_error e
  end

let%expect_test "ptr-specs:pointer-refs" =
  check_list PS.pointer_ref_of_string Sc_C.Printer.pp_pointer_ref
    ["a"; "{struct s}.f"; "{foo"];
  [%expect {|
    "a": a
    "{struct s}.f": {struct s}.f
    "{foo": syntax error in "{foo": expected pointer reference
    |}]
;;

let%expect_test "ptr-specs:pointer-constraints" =
  check_list PS.pointer_constraint_of_string Sc_C.Printer.pp_pointer_constraint
    ["a:n"; "{struct s}:p:n"; " { struct   s } :  p : NNN ";
     " a"; "{foo"; " { struct  }.f:n"];
  [%expect {|
    "a:n": a:n
    "{struct s}:p:n": {struct s}:p:n
    " { struct   s } :  p : NNN ": {struct s}:p:NNN
    " a": syntax error in " a": expected pointer constraint
    "{foo": syntax error in "{foo": expected pointer constraint
    " { struct  }.f:n": syntax error in " { struct  }.f:n": expected pointer
                        constraint
    |}]
;;
