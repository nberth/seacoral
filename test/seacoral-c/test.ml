(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

module AP = Sc_C.Access_path

let%expect_test "access-path:basic" =
  let orig = AP.origin_only "orig" in
  let orig_f1 = AP.append_field orig "f1" in
  let orig_f2 = AP.append_field orig "f2" in
  let orig_f1_f2 = AP.append_field orig_f1 "f2" in
  Fmt.pr "%a@\n" AP.print orig;
  Fmt.pr "%a@\n" AP.print orig_f1;
  Fmt.pr "%a@\n" AP.print orig_f2;
  Fmt.pr "%a@\n" AP.print orig_f1_f2;
  [%expect {|
    orig
    orig.f1
    orig.f2
    orig.f1.f2
    |}]
;;

let%expect_test "access-path:suffix-substitution" =
  let orig = AP.origin_only "orig" in
  let orig_f1 = AP.append_field orig "f1" in
  let orig_f1_f2 = AP.append_field orig_f1 "f2" in
  let f3 = AP.field "f3" in
  let orig_f1_f3 = AP.subst_rigthmost_suffix orig_f1_f2 f3 in
  Fmt.pr "%a@\n" AP.print orig_f1_f2;
  Fmt.pr "%a@\n" AP.print orig_f1_f3;
  [%expect {|
    orig.f1.f2
    orig.f1.f3
    |}]
;;

let%expect_test "access-path:origin-removal" =
  let _f1 = AP.field "f1" in
  let _f1_f2 = AP.append_to_suffix _f1 (AP.field "f2") in
  let f1 = AP.HACK.forget_first_suffix_punct _f1 in
  let f1_f2 = AP.HACK.forget_first_suffix_punct _f1_f2 in
  Fmt.pr "%a@\n" AP.print f1;
  Fmt.pr "%a@\n" AP.print f1_f2;
  [%expect {|
    f1
    f1.f2
    |}]
;;
