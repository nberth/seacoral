(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open !Base
open Basics
open Sc_values

module Format = Stdlib.Format
module Params = struct
  let encoding_params =
    {
      max_ptr_array_length = 1;
      max_non_nil_ptr_depth = 3;
      max_cstring_length = 20;
    }
end

module CilShortcuts = struct
  open GoblintCil.Cil
  let _i x = TInt (x, [])
  let _f x = TFloat (x, [])
  let bool = _i IBool
  let char = _i ISChar
  let short = _i IShort
  let int = _i IInt
  let long = _i ILong
  let signed_char = char
  let signed_short = short
  let signed_int = int
  let signed_long = long
  let unsigned_char = _i IUChar
  let unsigned_short = _i IUShort
  let unsigned_int = _i IUInt
  let unsigned_long = _i IULong
  let float = _f FFloat
  let double = _f FDouble
  let array t size = TArray (t, Some (integer size), [])
  let ptr t = TPtr (t, [])
  let struct_decl t fields =
    let compinfo = {cstruct = true; cname = t; ckey = 0; cfields = [];
                    cattr = []; cdefined = false; creferenced = false} in
    compinfo.cfields <-
      List.map ~f:begin fun (t, n, _) ->
        { fcomp = compinfo; fname = n; ftype = t; fbitfield = None; fattr = [];
          floc = Cil.builtinLoc }
      end fields;
    GCompTag (compinfo, Cil.builtinLoc)
  let struct_ t =                 (* only cstruct and cname are relevant here *)
    TComp ({cstruct = true; cname = t; ckey = 0; cfields = [];
            cattr = []; cdefined = false; creferenced = false}, [])
  let as_carray ?size (t, v) =
    Sc_C.Defs.as_pointer_to_carray ?size (t, v, [])
  let as_cstring (t, v) =                                     (* maybe broken *)
    Sc_C.Defs.as_pointer_to_cstring (t, v, [])
  let ( !@ ) = List.map ~f:(fun (t, v) -> t, v, [])
end
open !CilShortcuts

let postproc s =
  Stdlib.print_string s                           (* print on stdout directly *)

let pp_c_code (type v) (module Val: Sc_values.Struct.VAL with type t = v) (v: v) ~vars =
  let lit = Val.fields_as_c_literals v in
  let c_lit = Val.Printer.literal_memory_as_c_code ~globals:[] ~locals:vars lit in
  Fmt.pr "@[<2>literal:@\n";
  Fmt.pr "heap: @[%t@]@\n" (c_lit.pp_heap ~static:false);
  Fmt.pr "vars: @[%t@]@]@\n" c_lit.pp_locals;
  let als = Val.fields_as_c_allocations v in
  let c_als = Val.Printer.instructions_as_c_code ~globals:[] ~locals:vars als in
  Fmt.pr "@[<2>allocation/initialization:@\n";
  Fmt.pr "vars: @[%t@]@]@\n" c_als.pp_locals

let%expect_test "Struct { int x; float y }" =
  let open Struct in
  let open Repr (Params) in
  let vars = !@[ int, "x"; float, "y" ] in
  let typ = from_cil_fields "t" vars in
  [%test_eq: int] (size typ) 8;
  [%test_eq: int list] (padding_bytes typ) [0; 0];
  let v = Val.blank typ in
  Fmt.pr "%a: %S@." Val.print v (Val.to_string v);
  Val.assign_from_literal empty_typdecls v
    (LBMap StrMap.(empty |>
                   add "x" (LBStr "1") |>
                   add "y" (LBStr "42.")));
  Fmt.pr "%a@." Val.print v;
  pp_c_code (module Val) v ~vars;
  postproc [%expect.output];
  [%expect {|
    {.x = 0, .y = 0}: "\000\000\000\000\000\000\000\000"
    {.x = 1, .y = 42}
    literal:
      heap:
      vars: int x = 1;
            float y = 0x1.5p+5 /*42*/;
    allocation/initialization:
      vars: int x = 1;
            float y = 0x1.5p+5 /*42*/;
    |}]
;;

let%expect_test "Struct { char c; int i }" =                 (* tests padding *)
  let open Struct in
  let open Repr (Params) in
  let vars = !@[ char, "c"; int, "i" ] in
  let typ = from_cil_fields "t" vars in
  [%test_eq: int] (size typ) 8;
  [%test_eq: int list] (padding_bytes typ) [3; 0];
  let v = Val.blank typ in
  Fmt.pr "%S@." (Val.to_string v);
  Val.assign_from_literal empty_typdecls v
    (LBMap StrMap.(empty |>
                   add "c" (LBStr "'c'") |>
                   add "i" (LBStr "-1")));
  Fmt.pr "%a: %S@." Val.print v (Val.to_string v);
  let v = Val.of_string typ "d\255\255\255\214\255\255\255" in
  Fmt.pr "%a: %S@." Val.print v (Val.to_string v);
  pp_c_code (module Val) v ~vars;
  postproc [%expect.output];
  [%expect{|
    "\000\000\000\000\000\000\000\000"
    {.c = 'c', .i = -1}: "c\000\000\000\255\255\255\255"
    {.c = 'd', .i = -42}: "d\000\000\000\214\255\255\255"
    literal:
      heap:
      vars: char c = 'd';
            int i = -42;
    allocation/initialization:
      vars: char c = 'd';
            int i = -42;
    |}]
;;

let%expect_test "Struct { char c; int i; char d }" =   (* tests end-of-struct padding *)
  let open Struct in
  let open Repr (Params) in
  let vars = !@[ char, "c"; int, "i"; char, "d" ] in
  let typ = from_cil_fields "t" vars in
  [%test_eq: int] (size typ) 12;
  [%test_eq: int list] (padding_bytes typ) [3; 0; 3];
  let v = Val.blank typ in
  Fmt.pr "%S@." (Val.to_string v);
  let v = Val.of_string typ "d\255\255\255\214\255\255\255e\255\255\255" in
  Fmt.pr "%a: %S@." Val.print v (Val.to_string v);
  let v = Val.blank typ in
  Val.assign_from_literal empty_typdecls v
    (LBMap StrMap.(empty |>
                   add "c" (LBStr "'c'") |>
                   add "d" (LBStr "'d'") |>
                   add "i" (LBStr "-1")));
  Fmt.pr "%a: %S@." Val.print v (Val.to_string v);
  pp_c_code (module Val) v ~vars;
  postproc [%expect.output];
  [%expect{|
    "\000\000\000\000\000\000\000\000\000\000\000\000"
    {.c = 'd', .i = -42, .d = 'e'}: "d\000\000\000\214\255\255\255e\000\000\000"
    {.c = 'c', .i = -1, .d = 'd'}: "c\000\000\000\255\255\255\255d\000\000\000"
    literal:
      heap:
      vars: char c = 'c';
            int i = -1;
            char d = 'd';
    allocation/initialization:
      vars: char c = 'c';
            int i = -1;
            char d = 'd';
    |}]
;;

let%expect_test "Struct { int x[2]; float y }" =
  let open Struct in
  let open Repr (Params) in
  let vars = !@[ array int 2, "x"; float, "y" ] in
  let typ = from_cil_fields "t" vars in
  [%test_eq: int] (size typ) 12;
  [%test_eq: int list] (padding_bytes typ) [0; 0];
  let v = Val.blank typ in
  Fmt.pr "%a@." Val.print v;
  Val.assign_from_literal empty_typdecls v
    (LBMap StrMap.(empty |>
                   add "x" (LBLst [LBStr "2"; LBStr "-3"]) |>
                   add "y" (LBStr "42.")));
  Fmt.pr "%a@." Val.print v;
  pp_c_code (module Val) v ~vars;
  postproc [%expect.output];
  [%expect {|
    {.x = {0, 0}, .y = 0}
    {.x = {2, -3}, .y = 42}
    literal:
      heap:
      vars: int x[2];
            (void) memcpy (x, (int[2]){2, -3}, sizeof (int[2]));
            float y = 0x1.5p+5 /*42*/;
    allocation/initialization:
      vars: int x[2];
            (void) memcpy (x, (int[2]){2, -3}, sizeof (int[2]));
            float y = 0x1.5p+5 /*42*/;
    |}]
;;

let%expect_test "BoxedArray (unsigned char[3])" =
  let open BoxedArray in
  let open Repr (Params) in
  let typ = from_cil_elt_typ unsigned_char 3 in
  [%test_eq: int] (size typ) 3;
  let mem = "\x01\x02\x03" in
  let v = Val.of_string typ mem in
  [%test_eq: string] (Val.to_string v) mem;
  let mem0 = "\x00\x00\x00" in
  let blank = Val.blank typ in
  [%test_eq: string] (Val.to_string blank) mem0;
  Fmt.pr "%a@." Val.print v;
  Fmt.pr "%a@." Val.print blank;
  Val.assign_from_literal empty_typdecls blank
    (LBLst [LBStr "42"; LBStr "43"; LBStr "44"]);
  Fmt.pr "%a@." Val.print blank;
  postproc [%expect.output];
  [%expect {|
    {1, 2, 3}
    {0, 0, 0}
    {42, 43, 44}
    |}]
;;

let%expect_test "Struct with char field" =
  let open Struct in
  let open Repr (Params) in
  let typ = from_cil_fields "t" !@[ char, "c" ] in
  [%test_eq: int * int] (size_bounds typ) (1, 1);
  [%test_eq: int list] (padding_bytes typ) [0];
  Fmt.pr "%a@." definition typ;
  let mem = "c" in                                          (* encoded string *)
  let lit = "{.c = 'c'}" in
  let v = Val.of_string typ mem in
  [%test_eq: string] (Val.to_string v) mem;
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;

  let lit = "{.c = '\\''}" in
  let c_lit = "{.c = '\\''}" in
  Val.assign_from_literal empty_typdecls v
    (LBMap StrMap.(singleton "c" (LBStr "'\\''")));
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;
  [%test_eq: string] (fst @@ Val.as_c_literal v) c_lit;

  let lit = "{.c = '\\000'}" in
  let c_lit = "{.c = '\\000'}" in
  Val.assign_from_literal empty_typdecls v
    (LBMap StrMap.(singleton "c" (LBStr "'\000'")));
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;
  [%test_eq: string] (fst @@ Val.as_c_literal v) c_lit;
  Val.assign_from_literal empty_typdecls v
    (LBMap StrMap.(singleton "c" (LBStr "0")));
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;
  [%test_eq: string] (fst @@ Val.as_c_literal v) c_lit;

  let lit = "{.c = '\\001'}" in
  let c_lit = "{.c = '\\001'}" in
  Val.assign_from_literal empty_typdecls v
    (LBMap StrMap.(singleton "c" (LBStr "1")));
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;
  [%test_eq: string] (fst @@ Val.as_c_literal v) c_lit;

  let lit = "{.c = 255}" in
  let c_lit = "{.c = 255}" in
  Val.assign_from_literal empty_typdecls v
    (LBMap StrMap.(singleton "c" (LBStr "-1")));
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;
  [%test_eq: string] (fst @@ Val.as_c_literal v) c_lit;

  postproc [%expect.output];
  [%expect {| struct t { char c;  } |}]
;;

let%expect_test "Struct with char[7] field" =
  let open Struct in
  let open Repr (Params) in
  let typ = from_cil_fields "t" !@[ array char 7, "str" ] in
  [%test_eq: int * int] (size_bounds typ) (7, 7);
  [%test_eq: int list] (padding_bytes typ) [0];
  Fmt.pr "%a@." definition typ;
  let mem0 = "\x00\x00\x00\x00\x00\x00\x00" in              (* encoded string *)
  [%test_eq: string] (Val.to_string @@ Val.blank typ) mem0;
  let mem = "foobar\x00" in                                 (* encoded string *)
  let v = Val.of_string typ mem in
  [%test_eq: string] (Val.to_string v) mem;
  postproc [%expect.output];
  [%expect {| struct t { char str[7];  } |}]
;;

let%expect_test "Struct with int* field" =
  let open Struct in
  let open Repr (struct
      let encoding_params =
        { Params.encoding_params with max_ptr_array_length = 3 }
    end) in
  let vars = [ as_carray (ptr int, "x") ] in
  let typ = from_cil_fields "t" vars in
  [%test_eq: int * int] (size_bounds typ)
    (8, 8 + Params.encoding_params.max_ptr_array_length * 4);
  [%test_eq: int list] (padding_bytes typ) [0];
  Fmt.pr "%a@." definition typ;
  let ptr = "\x01\x00\x00\x00\x00\x00\x00\x00" in          (* encoded pointer *)
  let i = "\042\x00\x00\x00" in
  let mem = ptr ^ i in
  let v = Val.of_string typ mem in
  [%test_eq: string] (Val.to_string v) mem;
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) "{.x = &42}";
  Stdlib.Gc.full_major ();
  [%test_eq: string] (Val.to_string v) mem;
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) "{.x = &42}";
  Stdlib.Gc.full_major ();
  [%test_eq: string] (Val.to_string v) mem;
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) "{.x = &42}";

  pp_c_code (module Val) v ~vars;

  let lit = "{.x = &{42, 24}}" in
  let ptr = "\x02\x00\x00\x00\x00\x00\x00\x00" in        (* encoded pointer *)
  let i0 = "\042\x00\x00\x00" in
  let i1 = "\024\x00\x00\x00" in
  let mem = ptr ^ i0 ^ i1 in
  let v = Val.of_string typ mem in
  [%test_eq: string] (Val.to_string v) mem;
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;
  Stdlib.Gc.full_major ();
  [%test_eq: string] (Val.to_string v) mem;
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;

  pp_c_code (module Val) v ~vars;

  let lit = "{.x = NULL}" in
  Val.assign_from_literal empty_typdecls v
    (LBMap (StrMap.singleton "x" LBNil));
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;

  let lit = "{.x = &22}" in
  Val.assign_from_literal empty_typdecls v
    (LBMap (StrMap.singleton "x" (LBRef (LBStr "22"))));
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;

  let lit = "{.x = &33}" in
  Val.assign_from_literal empty_typdecls v
    (LBMap (StrMap.singleton "x" (LBRef (LBStr "33"))));
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;

  let lit = "{.x = &{1, 2, 3}}" in
  let lb_arr = LBArr [|LBStr "1"; LBStr "2"; LBStr "3"|] in
  Val.assign_from_literal empty_typdecls v
    (LBMap (StrMap.singleton "x" (LBRef lb_arr)));
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;

  let lit = "{.x = &{3, 2, 1}}" in
  let lb_arr = LBArr [|LBStr "3"; LBStr "2"; LBStr "1"|] in
  Val.assign_from_literal empty_typdecls v
    (LBMap (StrMap.singleton "x" (LBRef lb_arr)));
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;

  let lit = "{.x = &{2, 1}}" in
  let lb_arr = LBArr [|LBStr "2"; LBStr "1"|] in
  Val.assign_from_literal empty_typdecls v
    (LBMap (StrMap.singleton "x" (LBRef lb_arr)));
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;

  let lit = "{.x = NULL}" in
  Val.assign_from_literal empty_typdecls v
    (LBMap (StrMap.singleton "x" LBNil));
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;

  postproc [%expect.output];
  [%expect {|
    struct t { int* x;  }
    literal:
      heap: int _heap_obj_0_;
            _heap_obj_0_ = 42;
      vars: int (* x) = &_heap_obj_0_;
    allocation/initialization:
      vars: int (* x) = malloc (sizeof (int[1]));
            (void) memcpy (x, (int[1]){42}, sizeof (int[1]));
    literal:
      heap: int _heap_obj_0_[2];
            (void) memcpy (&(_heap_obj_0_), (int[2]){42, 24}, sizeof (int[2]));
      vars: int (* x) = _heap_obj_0_;
    allocation/initialization:
      vars: int (* x) = malloc (sizeof (int[2]));
            (void) memcpy (x, (int[2]){42, 24}, sizeof (int[2]));
    |}]
;;

let%expect_test "Struct with a bool field" =
  let open Struct in
  let open Repr (Params) in
  let typ = from_cil_fields "t" !@[ bool, "b" ] in
  [%test_eq: int * int] (size_bounds typ) (1, 1);
  [%test_eq: int list] (padding_bytes typ) [0];
  Fmt.pr "%a@." definition typ;
  let mem = "\x01" in
  let v = Val.of_string typ mem in
  [%test_eq: string] (Val.to_string v) mem;

  let lit = "{.b = false}" in
  Val.assign_from_literal empty_typdecls v
    (LBMap (StrMap.singleton "b" (LBStr "FALSE")));
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;

  let lit = "{.b = 7}"
  and c_lit = "{.b = 7}" in
  Val.assign_from_literal empty_typdecls v
    (LBMap (StrMap.singleton "b" (LBStr "0b0000111")));
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;
  [%test_eq: string] (fst @@ Val.as_c_literal v) c_lit;

  postproc [%expect.output];
  [%expect {| struct t { _Bool b;  } |}]
;;

let%expect_test "Struct with bool[3] field" =
  let open Struct in
  let open Repr (Params) in
  let typ = from_cil_fields "t" !@[ array bool 3, "bools" ] in
  [%test_eq: int * int] (size_bounds typ) (3, 3);
  [%test_eq: int list] (padding_bytes typ) [0];
  Fmt.pr "%a@." definition typ;
  let mem = "\x00\x01\x00" in
  let v = Val.of_string typ mem in
  [%test_eq: string] (Val.to_string v) mem;

  let lit = "{.bools = {false, true, false}}" in
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;

  let lit = "{.bools = {false, false, true}}" in
  let lb_arr = LBArr [|LBStr "FALSE"; LBStr "FALSE"; LBStr "TRUE"|] in
  Val.assign_from_literal empty_typdecls v
    (LBMap (StrMap.singleton "bools" lb_arr));
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;

  let mem = "\xff\x00\x42" in                 (* bit patterns remain the same *)
  let v = Val.of_string typ mem in
  [%test_eq: string] (Val.to_string v) mem;

  postproc [%expect.output];
  [%expect {| struct t { _Bool bools[3];  } |}]
;;

let pad (module Params: ENCODING_PARAMS) ?actual_length s =
  let len = match actual_length with Some l -> l | None -> String.length s in
  let max_len = Params.encoding_params.max_cstring_length in
  String.concat "" @@ s :: List.init ~f:(fun _ -> "\\000") (max_len - len)

let%expect_test "Struct with char* field" =
  let open Struct in
  let open Repr (Params) in
  let pad = pad (module Params) in
  let vars = [ as_cstring (ptr char, "str") ] in
  let typ = from_cil_fields "t" vars in
  [%test_eq: int * int] (size_bounds typ) (8, 8 + 20);
  [%test_eq: int list] (padding_bytes typ) [0];
  Fmt.pr "%a@." definition typ;

  let lit = "{.str = \"foobar\"}"
  and c_lit = Format.asprintf "{.str = \"%s\"}" @@ pad "foobar" in
  let ptr = "\x01\x00\x00\x00\x00\x00\x00\x00"             (* encoded pointer *)
  and str = "foobar\x00\x00\x00\x00\
             \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00" in (* encoded char[20] *)
  let mem = ptr ^ str in
  let v = Val.of_string typ mem in
  [%test_eq: string] (Val.to_string v) mem;
  [%test_eq: string] (Format.asprintf "%a" Val.print v) lit;
  [%test_eq: string] (fst @@ Val.as_c_literal v) c_lit;
  Stdlib.Gc.full_major ();
  [%test_eq: string] (Format.asprintf "%a" Val.print v) lit;
  [%test_eq: string] (fst @@ Val.as_c_literal v) c_lit;

  pp_c_code (module Val) v ~vars;

  let lit = "{.str = NULL}" in
  Val.assign_from_literal empty_typdecls v
    (LBMap (StrMap.singleton "str" LBNil));
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;
  [%test_eq: string] (fst @@ Val.as_c_literal v) lit;
  Stdlib.Gc.full_major ();
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;
  [%test_eq: string] (fst @@ Val.as_c_literal v) lit;

  pp_c_code (module Val) v ~vars;

  let lit = "{.str = \"foo\"}"
  and c_lit = Format.asprintf "{.str = \"%s\"}" @@ pad "foo" in
  let ptr = "\x01\x00\x00\x00\x00\x00\x00\x00"             (* encoded pointer *)
  and str = "foo\x00\x00\x00\x00\x00\x00\x00\
             \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00" in (* encoded char[20] *)
  let mem = ptr ^ str in
  Val.assign_from_literal empty_typdecls v
    (LBMap (StrMap.singleton "str" (LBStr "foo")));
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;
  [%test_eq: string] (fst @@ Val.as_c_literal v) c_lit;
  [%test_eq: string] (Val.to_string v) mem;
  Stdlib.Gc.full_major ();
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;
  [%test_eq: string] (fst @@ Val.as_c_literal v) c_lit;
  [%test_eq: string] (Val.to_string v) mem;

  let lit = "{.str = \"\"}"
  and c_lit = Format.asprintf "{.str = \"%s\"}" @@ pad "" in
  let ptr = "\x01\x00\x00\x00\x00\x00\x00\x00"             (* encoded pointer *)
  and str = "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
             \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00" in (* encoded char[20] *)
  let mem = ptr ^ str in
  Val.assign_from_literal empty_typdecls v
    (LBMap (StrMap.singleton "str" (LBStr "")));
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;
  [%test_eq: string] (fst @@ Val.as_c_literal v) c_lit;
  [%test_eq: string] (Val.to_string v) mem;
  Stdlib.Gc.full_major ();
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;
  [%test_eq: string] (fst @@ Val.as_c_literal v) c_lit;
  [%test_eq: string] (Val.to_string v) mem;

  pp_c_code (module Val) v ~vars;

  (* Note there is still symmetry between pseudo-lierals (`Val.print`) and
     literal bindings. *)
  let lit = "{.str = \"bar\\001\\002\"}"
  and c_lit = (Format.asprintf "{.str = \"%s\"}" @@
    pad ~actual_length:5 "bar\\001\\002") in
  Val.assign_from_literal empty_typdecls v
    (LBMap (StrMap.singleton "str" (LBStr "bar\x01\x02")));
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;
  [%test_eq: string] (fst @@ Val.as_c_literal v) c_lit;
  Stdlib.Gc.full_major ();
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;
  [%test_eq: string] (fst @@ Val.as_c_literal v) c_lit;
  (* Val.assign_from_literal empty_typdecls v *)
  (*   (LBMap (StrMap.singleton "str" *)
  (*             (LBLst [LBStr "b"; LBStr "a"; LBStr "r"; *)
  (*                     LBStr "\x01"; LBStr "\x02"; LBStr "\000"]))); *)
  (* [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit; *)
  (* [%test_eq: string] (fst @@ Val.as_c_literal v) c_lit; *)
  (* Stdlib.Gc.full_major (); *)
  (* [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit; *)
  (* [%test_eq: string] (fst @@ Val.as_c_literal v) c_lit; *)

  postproc [%expect.output];
  [%expect {|
    struct t { char* str;  }
    literal:
      heap:
      vars: char str[21];
            (void) memcpy (str,
                           "foobar\000\000\000\000\000\000\000\000\000\000\000\000\000\000",
                           sizeof (char[21]));
    allocation/initialization:
      vars: char (* str) = malloc (sizeof (char[21]));
            (void) memcpy (str,
                           "foobar\000\000\000\000\000\000\000\000\000\000\000\000\000\000",
                           sizeof (char[21]));
    literal:
      heap:
      vars: char (* str) = NULL;
    allocation/initialization:
      vars: char (* str) = NULL;
    literal:
      heap:
      vars: char str[21];
            (void) memcpy (str,
                           "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000",
                           sizeof (char[21]));
    allocation/initialization:
      vars: char (* str) = malloc (sizeof (char[21]));
            (void) memcpy (str,
                           "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000",
                           sizeof (char[21]));
    |}]
;;

let ptr_size = Ctypes.(sizeof (ptr void))
let int_size = Ctypes.(sizeof int)
let char_size = Ctypes.(sizeof char)

let%expect_test "Struct with an int* field constrained with an int" =
  let open Struct in
  let open Repr (struct
      let encoding_params =
        { Params.encoding_params with max_ptr_array_length = 3 }
    end) in
  let vars = [ as_carray ~size:(`Var "n") (ptr int, "x"); int, "n", [] ] in
  let typ = from_cil_fields "t" vars in
  [%test_eq: int * int] (size_bounds typ)
    (ptr_size + int_size + 4,
     ptr_size + int_size + 4 + Params.encoding_params.max_ptr_array_length * 4);
  [%test_eq: int list] (padding_bytes typ) [0; 4];
  Fmt.pr "%a@." definition typ;
  let ptr     = "\x00\x00\x00\x00\x00\x00\x00\x00" in         (* zero => NULL *)
  let n       = "\xde\xad\xbe\xaf" in    (* becomes zero after normalization. *)
  let padding = "\000\x00\x00\x00" in
  let mem = ptr ^ n ^ padding in
  let v = Val.of_string typ mem in
  [%test_eq: string] (Val.to_string v)
    "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000";
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v)
    "{.x = NULL, .n = 0}";

  pp_c_code (module Val) v ~vars;

  let ptr     = "\x01\x00\x00\x00\x00\x00\x00\x00" in             (* non-zero *)
  let n       = "\001\x00\x00\x00" in
  let padding = "\000\x00\x00\x00" in
  let i       = "\042\x00\x00\x00" in
  let mem = ptr ^ n ^ padding ^ i in
  let v = Val.of_string typ mem in
  [%test_eq: string] (Val.to_string v) mem;
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v)
    "{.x = &42, .n = 1}";

  pp_c_code (module Val) v ~vars;

  postproc [%expect.output];
  [%expect {|
    struct t { int* x; int n;  }
    literal:
      heap:
      vars: int (* x) = NULL;
            int n = 0;
    allocation/initialization:
      vars: int (* x) = NULL;
            int n = 0;
    literal:
      heap: int _heap_obj_0_;
            _heap_obj_0_ = 42;
      vars: int (* x) = &_heap_obj_0_;
            int n = 1;
    allocation/initialization:
      vars: int (* x) = malloc (sizeof (int[1]));
            (void) memcpy (x, (int[1]){42}, sizeof (int[1]));
            int n = 1;
    |}]
;;

let%expect_test "Struct with an int* field constrained with a char" =
  let open Struct in
  let open Repr (struct
      let encoding_params =
        { Params.encoding_params with max_ptr_array_length = 3 }
    end) in
  let vars = [ as_carray ~size:(`Var "c") (ptr int, "x"); char, "c", [] ] in
  let typ = from_cil_fields "t" vars in
  [%test_eq: int * int] (size_bounds typ)
    (ptr_size + char_size + 7,
     ptr_size + char_size + 7 +
     Params.encoding_params.max_ptr_array_length * 4);
  [%test_eq: int list] (padding_bytes typ) [0; 7];
  Fmt.pr "%a@." definition typ;
  let ptr     = "\x01\x00\x00\x00\x00\x00\x00\x00" in             (* non-zero *)
  let c       = "\002" in
  let padding = "\x00\x00\x00\000\x00\x00\x00" in
  let i       = "\042\x00\x00\x00" in
  let j       = "\043\x00\x00\x00" in
  let mem = ptr ^ c ^ padding ^ i ^ j in
  let v = Val.of_string typ mem in
  [%test_eq: string] (Val.to_string v) mem;
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v)
    "{.x = &{42, 43}, .c = '\\002'}";

  pp_c_code (module Val) v ~vars;

  postproc [%expect.output];
  [%expect {|
    struct t { int* x; char c;  }
    literal:
      heap: int _heap_obj_0_[2];
            (void) memcpy (&(_heap_obj_0_), (int[2]){42, 43}, sizeof (int[2]));
      vars: int (* x) = _heap_obj_0_;
            char c = '\002';
    allocation/initialization:
      vars: int (* x) = malloc (sizeof (int[2]));
            (void) memcpy (x, (int[2]){42, 43}, sizeof (int[2]));
            char c = '\002';
    |}]
;;

let%expect_test "List of ints" =
  let open Struct in
  let max_non_nil_ptr_depth = 2 in
  let open Repr (struct
      let encoding_params =
        { Params.encoding_params with max_non_nil_ptr_depth }
    end)
  in
  let typdecls =
    Sc_values.empty_typdecls |>
    Sc_values.append_global_cil_declaration_to_typdecls
      (struct_decl "list" !@[ ptr (struct_ "list"), "next"; int, "x" ])
  in
  let vars = !@[ ptr (struct_ "list"), "list" ] in
  let typ = from_cil_fields ~typdecls "t" vars in
  [%test_eq: int * int] (size_bounds typ)
    (8, (*t*)8 + max_non_nil_ptr_depth * ((*next*)8 + (*int+<padding>*)8));
  [%test_eq: int list] (padding_bytes typ) [0];
  Fmt.pr "%a@." definition typ;
  let v = Val.blank typ in

  let lit = "{.list = NULL}" in
  Val.assign_from_literal empty_typdecls v
    (LBMap (StrMap.singleton "list" LBNil));
  [%test_eq: string] (Stdlib.Format.asprintf "%a" Val.print v) lit;
  Stdlib.print_newline ();
  Stdlib.print_endline "- empty list:";
  pp_c_code (module Val) v ~vars;

  Stdlib.print_newline ();
  Stdlib.print_endline "- 1-element list:";
  Val.assign_from_literal empty_typdecls v
    (LBMap (StrMap.of_list [
         "list", LBRef (LBMap (StrMap.of_list [
             "x", LBStr "42";
             "next", LBNil;
           ]));
       ]));
  pp_c_code (module Val) v ~vars;

  Stdlib.print_newline ();
  Stdlib.print_endline "- 2-element list:";
  Val.assign_from_literal empty_typdecls v
    (LBMap (StrMap.of_list [
         "list", LBRef (LBMap (StrMap.of_list [
             "x", LBStr "42";
             "next", LBRef (LBMap (StrMap.of_list [
                 "x", LBStr "43";
                 "next", LBNil;
               ]));
           ]));
       ]));
  pp_c_code (module Val) v ~vars;

  postproc [%expect.output];
  [%expect {|
    struct t { struct list* list;  }

    - empty list:
    literal:
      heap:
      vars: struct list (* list) = NULL;
    allocation/initialization:
      vars: struct list (* list) = NULL;

    - 1-element list:
    literal:
      heap: struct list _heap_obj_0_;
            (void) memcpy (&(_heap_obj_0_), (struct list){.next = NULL, .x = 42},
                           sizeof (struct list));
      vars: struct list (* list) = &_heap_obj_0_;
    allocation/initialization:
      vars: struct list (* list) = malloc (sizeof (struct list[1]));
            list[0].next = NULL;
            list[0].x = 42;

    - 2-element list:
    literal:
      heap: struct list _heap_obj_0_;
            struct list _heap_obj_1_;
            (void) memcpy (&(_heap_obj_0_), (struct list){.next = NULL, .x = 43},
                           sizeof (struct list));
            (void) memcpy (&(_heap_obj_1_),
                           (struct list){.next = &_heap_obj_0_, .x = 42},
                           sizeof (struct list));
      vars: struct list (* list) = &_heap_obj_1_;
    allocation/initialization:
      vars: struct list (* list) = malloc (sizeof (struct list[1]));
            list[0].next = malloc (sizeof (struct list[1]));
            list[0].next[0].next = NULL;
            list[0].next[0].x = 43;
            list[0].x = 42;
    |}]
;;

let%expect_test "Bits of everything, mostly to check output of values that \
                 involve various types as C code" =
  let open Struct in
  let max_non_nil_ptr_depth = 2
  and max_ptr_array_length = 2
  and max_cstring_length = 20 in
  let open Repr (struct
      let encoding_params =
        { max_non_nil_ptr_depth;
          max_cstring_length;
          max_ptr_array_length }
    end)
  in
  let typdecls =
    Sc_values.empty_typdecls |>
    Sc_values.append_global_cil_declarations_to_typdecls
      [ struct_decl "s1" [ as_carray (ptr (struct_ "s1"), "s11");
                           int, "a", [];
                           bool, "b", [];
                           as_cstring (ptr char, "c"); ];
        struct_decl "s2" [ as_cstring (ptr (struct_ "s1"), "s12");
                           float, "d", [];
                           as_carray (ptr char, "e"); ] ]
  in
  let vars = [ as_carray (ptr (struct_ "s1"), "s1");
               as_carray ~size:(`Var "n") (ptr (struct_ "s2"), "s2");
               int, "n", [];
               as_cstring (ptr char, "f"); ]
  in
  let typ = from_cil_fields ~typdecls "t" vars in
  Fmt.pr "%a@." definition typ;
  let v = Val.blank typ in
  Fmt.pr "%a: %S@." Val.print v (Val.to_string v);
  pp_c_code (module Val) v ~vars;

  let rec enter ap i =
    Fmt.pr ">>%*s _%a@." i "" (Fmt.option Sc_C.Access_path.print_suffix) ap;
    Do_children_and_then (i + 1, leave ap)
  and leave ap i =
    let i = i - 1 in
    Fmt.pr "<<%*s _%a@." i "" (Fmt.option Sc_C.Access_path.print_suffix) ap;
    i
  in
  ignore @@ visit_access_paths typ 0
    ~f_prm:{ f = fun _typ -> enter }
    ~f_ptr:{ f = fun _typ ap _attrs -> enter ap };
  ignore @@ visit_access_paths ~max_depth:0 typ 0
    ~f_prm:{ f = fun _typ -> enter }
    ~f_ptr:{ f = fun _typ ap _attrs -> enter ap };
  ignore @@ visit_access_paths ~max_depth:1 typ 0
    ~f_prm:{ f = fun _typ -> enter }
    ~f_ptr:{ f = fun _typ ap _attrs -> enter ap };

  let ones = String.init (snd @@ size_bounds typ) (fun _ -> '\001') in
  let v = Val.of_string typ ones in
  Fmt.pr "%a: %S@." Val.print v (Val.to_string v);
  pp_c_code (module Val) v ~vars;

  let twos = String.init (snd @@ size_bounds typ) (fun _ -> '\002') in
  let v = Val.of_string typ twos in
  Fmt.pr "%a: %S@." Val.print v (Val.to_string v);
  pp_c_code (module Val) v ~vars;

  postproc [%expect.output];
  [%expect {|
    struct t { struct s1* s1; struct s2* s2; int n; char* f;  }
    {.s1 = NULL, .s2 = NULL, .n = 0, .f = NULL}: "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
    literal:
      heap:
      vars: struct s1 (* s1) = NULL;
            struct s2 (* s2) = NULL;
            int n = 0;
            char (* f) = NULL;
    allocation/initialization:
      vars: struct s1 (* s1) = NULL;
            struct s2 (* s2) = NULL;
            int n = 0;
            char (* f) = NULL;
    >> _
    >>  _.s1
    >>   _.s1[0]
    >>    _.s1[0].s11
    >>     _.s1[0].s11[0]
    >>      _.s1[0].s11[0].s11
    <<      _.s1[0].s11[0].s11
    >>      _.s1[0].s11[0].a
    <<      _.s1[0].s11[0].a
    >>      _.s1[0].s11[0].b
    <<      _.s1[0].s11[0].b
    >>      _.s1[0].s11[0].c
    <<      _.s1[0].s11[0].c
    <<     _.s1[0].s11[0]
    <<    _.s1[0].s11
    >>    _.s1[0].a
    <<    _.s1[0].a
    >>    _.s1[0].b
    <<    _.s1[0].b
    >>    _.s1[0].c
    >>     _.s1[0].c[0]
    <<     _.s1[0].c[0]
    <<    _.s1[0].c
    <<   _.s1[0]
    >>   _.s1[1]
    >>    _.s1[1].s11
    >>     _.s1[1].s11[0]
    >>      _.s1[1].s11[0].s11
    <<      _.s1[1].s11[0].s11
    >>      _.s1[1].s11[0].a
    <<      _.s1[1].s11[0].a
    >>      _.s1[1].s11[0].b
    <<      _.s1[1].s11[0].b
    >>      _.s1[1].s11[0].c
    <<      _.s1[1].s11[0].c
    <<     _.s1[1].s11[0]
    <<    _.s1[1].s11
    >>    _.s1[1].a
    <<    _.s1[1].a
    >>    _.s1[1].b
    <<    _.s1[1].b
    >>    _.s1[1].c
    >>     _.s1[1].c[0]
    <<     _.s1[1].c[0]
    <<    _.s1[1].c
    <<   _.s1[1]
    <<  _.s1
    >>  _.s2
    >>   _.s2[0]
    >>    _.s2[0].s12
    >>     _.s2[0].s12[0]
    >>      _.s2[0].s12[0].s11
    <<      _.s2[0].s12[0].s11
    >>      _.s2[0].s12[0].a
    <<      _.s2[0].s12[0].a
    >>      _.s2[0].s12[0].b
    <<      _.s2[0].s12[0].b
    >>      _.s2[0].s12[0].c
    <<      _.s2[0].s12[0].c
    <<     _.s2[0].s12[0]
    <<    _.s2[0].s12
    >>    _.s2[0].d
    <<    _.s2[0].d
    >>    _.s2[0].e
    >>     _.s2[0].e[0]
    <<     _.s2[0].e[0]
    <<    _.s2[0].e
    <<   _.s2[0]
    >>   _.s2[1]
    >>    _.s2[1].s12
    >>     _.s2[1].s12[0]
    >>      _.s2[1].s12[0].s11
    <<      _.s2[1].s12[0].s11
    >>      _.s2[1].s12[0].a
    <<      _.s2[1].s12[0].a
    >>      _.s2[1].s12[0].b
    <<      _.s2[1].s12[0].b
    >>      _.s2[1].s12[0].c
    <<      _.s2[1].s12[0].c
    <<     _.s2[1].s12[0]
    <<    _.s2[1].s12
    >>    _.s2[1].d
    <<    _.s2[1].d
    >>    _.s2[1].e
    >>     _.s2[1].e[0]
    <<     _.s2[1].e[0]
    <<    _.s2[1].e
    <<   _.s2[1]
    <<  _.s2
    >>  _.n
    <<  _.n
    >>  _.f
    >>   _.f[0]
    <<   _.f[0]
    >>   _.f[1]
    <<   _.f[1]
    >>   _.f[2]
    <<   _.f[2]
    >>   _.f[3]
    <<   _.f[3]
    >>   _.f[4]
    <<   _.f[4]
    >>   _.f[5]
    <<   _.f[5]
    >>   _.f[6]
    <<   _.f[6]
    >>   _.f[7]
    <<   _.f[7]
    >>   _.f[8]
    <<   _.f[8]
    >>   _.f[9]
    <<   _.f[9]
    >>   _.f[10]
    <<   _.f[10]
    >>   _.f[11]
    <<   _.f[11]
    >>   _.f[12]
    <<   _.f[12]
    >>   _.f[13]
    <<   _.f[13]
    >>   _.f[14]
    <<   _.f[14]
    >>   _.f[15]
    <<   _.f[15]
    >>   _.f[16]
    <<   _.f[16]
    >>   _.f[17]
    <<   _.f[17]
    >>   _.f[18]
    <<   _.f[18]
    >>   _.f[19]
    <<   _.f[19]
    <<  _.f
    << _
    >> _
    >>  _.s1
    <<  _.s1
    >>  _.s2
    <<  _.s2
    >>  _.n
    <<  _.n
    >>  _.f
    <<  _.f
    << _
    >> _
    >>  _.s1
    >>   _.s1[0]
    >>    _.s1[0].s11
    <<    _.s1[0].s11
    >>    _.s1[0].a
    <<    _.s1[0].a
    >>    _.s1[0].b
    <<    _.s1[0].b
    >>    _.s1[0].c
    <<    _.s1[0].c
    <<   _.s1[0]
    >>   _.s1[1]
    >>    _.s1[1].s11
    <<    _.s1[1].s11
    >>    _.s1[1].a
    <<    _.s1[1].a
    >>    _.s1[1].b
    <<    _.s1[1].b
    >>    _.s1[1].c
    <<    _.s1[1].c
    <<   _.s1[1]
    <<  _.s1
    >>  _.s2
    >>   _.s2[0]
    >>    _.s2[0].s12
    <<    _.s2[0].s12
    >>    _.s2[0].d
    <<    _.s2[0].d
    >>    _.s2[0].e
    <<    _.s2[0].e
    <<   _.s2[0]
    >>   _.s2[1]
    >>    _.s2[1].s12
    <<    _.s2[1].s12
    >>    _.s2[1].d
    <<    _.s2[1].d
    >>    _.s2[1].e
    <<    _.s2[1].e
    <<   _.s2[1]
    <<  _.s2
    >>  _.n
    <<  _.n
    >>  _.f
    >>   _.f[0]
    <<   _.f[0]
    >>   _.f[1]
    <<   _.f[1]
    >>   _.f[2]
    <<   _.f[2]
    >>   _.f[3]
    <<   _.f[3]
    >>   _.f[4]
    <<   _.f[4]
    >>   _.f[5]
    <<   _.f[5]
    >>   _.f[6]
    <<   _.f[6]
    >>   _.f[7]
    <<   _.f[7]
    >>   _.f[8]
    <<   _.f[8]
    >>   _.f[9]
    <<   _.f[9]
    >>   _.f[10]
    <<   _.f[10]
    >>   _.f[11]
    <<   _.f[11]
    >>   _.f[12]
    <<   _.f[12]
    >>   _.f[13]
    <<   _.f[13]
    >>   _.f[14]
    <<   _.f[14]
    >>   _.f[15]
    <<   _.f[15]
    >>   _.f[16]
    <<   _.f[16]
    >>   _.f[17]
    <<   _.f[17]
    >>   _.f[18]
    <<   _.f[18]
    >>   _.f[19]
    <<   _.f[19]
    <<  _.f
    << _
    {.s1 =
       &{{.s11 = &{.s11 = NULL, .a = 16843009, .b = true, .c = NULL},
          .a = 16843009, .b = true, .c = &'\001'},
         {.s11 = &{.s11 = NULL, .a = 16843009, .b = true, .c = NULL},
          .a = 16843009, .b = true, .c = &'\001'}},
     .s2 =
       &{.s12 = &{.s11 = NULL, .a = 16843009, .b = true, .c = NULL},
         .d = 2.36942782762e-38, .e = &'\001'}, .n = 1,
     .f =
       "\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001"}: "\002\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\001\001\001\001\001\001\001\001\001\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\001\001\001\001\001\001\001\001\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\001\001\001\001\001\001\001\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\001\001\001\001\001\001\001\001\000\000\000\000\000\000\000\000\001\001\000\000\000\000\000\000\000\001\001\001\001\001\001\001\001\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\001\001\001\001\001\001\001\000\000\000\000\000\000\000\000\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001"
    literal:
      heap: struct s1 _heap_obj_5_;
            char _heap_obj_6_;
            struct s2 _heap_obj_7_;
            struct s1 _heap_obj_0_;
            char _heap_obj_1_;
            struct s1 _heap_obj_2_;
            char _heap_obj_3_;
            struct s1 _heap_obj_4_[2];
            (void) memcpy (&(_heap_obj_5_),
                           (struct s1){.s11 = NULL, .a = 16843009, .b = true, .c = NULL},
                           sizeof (struct s1));
            _heap_obj_6_ = '\001';
            (void) memcpy (&(_heap_obj_7_),
                           (struct s2){.s12 = &_heap_obj_5_, .d = 0x1.020202p-125 /*2.36943e-38*/, .e = &_heap_obj_6_},
                           sizeof (struct s2));
            (void) memcpy (&(_heap_obj_0_),
                           (struct s1){.s11 = NULL, .a = 16843009, .b = true, .c = NULL},
                           sizeof (struct s1));
            _heap_obj_1_ = '\001';
            (void) memcpy (&(_heap_obj_2_),
                           (struct s1){.s11 = NULL, .a = 16843009, .b = true, .c = NULL},
                           sizeof (struct s1));
            _heap_obj_3_ = '\001';
            (void) memcpy (&(_heap_obj_4_),
                           (struct s1[2]){{.s11 = &_heap_obj_0_, .a = 16843009, .b = true, .c = &_heap_obj_1_}, {.s11 = &_heap_obj_2_, .a = 16843009, .b = true, .c = &_heap_obj_3_}},
                           sizeof (struct s1[2]));
      vars: struct s1 (* s1) = _heap_obj_4_;
            struct s2 (* s2) = &_heap_obj_7_;
            int n = 1;
            char f[21];
            (void) memcpy (f,
                           "\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001",
                           sizeof (char[21]));
    allocation/initialization:
      vars: struct s1 (* s1) = malloc (sizeof (struct s1[2]));
            s1[0].s11 = malloc (sizeof (struct s1[1]));
            s1[0].s11[0].s11 = NULL;
            s1[0].s11[0].a = 16843009;
            s1[0].s11[0].b = true;
            s1[0].s11[0].c = NULL;
            s1[0].a = 16843009;
            s1[0].b = true;
            s1[0].c = malloc (sizeof (char[1]));
            (void) memcpy (s1[0].c, (char[1]){'\001'}, sizeof (char[1]));
            s1[1].s11 = malloc (sizeof (struct s1[1]));
            s1[1].s11[0].s11 = NULL;
            s1[1].s11[0].a = 16843009;
            s1[1].s11[0].b = true;
            s1[1].s11[0].c = NULL;
            s1[1].a = 16843009;
            s1[1].b = true;
            s1[1].c = malloc (sizeof (char[1]));
            (void) memcpy (s1[1].c, (char[1]){'\001'}, sizeof (char[1]));
            struct s2 (* s2) = malloc (sizeof (struct s2[1]));
            s2[0].s12 = malloc (sizeof (struct s1[1]));
            s2[0].s12[0].s11 = NULL;
            s2[0].s12[0].a = 16843009;
            s2[0].s12[0].b = true;
            s2[0].s12[0].c = NULL;
            s2[0].d = 0x1.020202p-125 /*2.36943e-38*/;
            s2[0].e = malloc (sizeof (char[1]));
            (void) memcpy (s2[0].e, (char[1]){'\001'}, sizeof (char[1]));
            int n = 1;
            char (* f) = malloc (sizeof (char[21]));
            (void) memcpy (f,
                           "\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001",
                           sizeof (char[21]));
    {.s1 = &{.s11 = &{}, .a = 33686018, .b = 2, .c = &{}},
     .s2 =
       &{{.s12 = &{}, .d = 9.55146781436e-38, .e = &{}},
         {.s12 = &{}, .d = 9.55146781436e-38, .e = &{}}}, .n = 2, .f = NULL}: "\001\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\255\255\002\002\002\002\002\002\002\002\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\002\002\002\002\002\002\002\002\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\002\002\002\002\002\002\002\002\255\255\255\255\255\255\255\255"
    literal:
      heap: struct s1 _heap_obj_3_[0];
            char _heap_obj_4_[0];
            struct s1 _heap_obj_5_[0];
            char _heap_obj_6_[0];
            struct s2 _heap_obj_7_[2];
            struct s1 _heap_obj_0_[0];
            char _heap_obj_1_[0];
            struct s1 _heap_obj_2_;
            (void) memcpy (&(_heap_obj_3_), (struct s1[0]){},
                           sizeof (struct s1[0]));
            (void) memcpy (&(_heap_obj_4_), (char[0]){}, sizeof (char[0]));
            (void) memcpy (&(_heap_obj_5_), (struct s1[0]){},
                           sizeof (struct s1[0]));
            (void) memcpy (&(_heap_obj_6_), (char[0]){}, sizeof (char[0]));
            (void) memcpy (&(_heap_obj_7_),
                           (struct s2[2]){{.s12 = _heap_obj_3_, .d = 0x1.040404p-123 /*9.55147e-38*/, .e = _heap_obj_4_}, {.s12 = _heap_obj_5_, .d = 0x1.040404p-123 /*9.55147e-38*/, .e = _heap_obj_6_}},
                           sizeof (struct s2[2]));
            (void) memcpy (&(_heap_obj_0_), (struct s1[0]){},
                           sizeof (struct s1[0]));
            (void) memcpy (&(_heap_obj_1_), (char[0]){}, sizeof (char[0]));
            (void) memcpy (&(_heap_obj_2_),
                           (struct s1){.s11 = _heap_obj_0_, .a = 33686018, .b = 2, .c = _heap_obj_1_},
                           sizeof (struct s1));
      vars: struct s1 (* s1) = &_heap_obj_2_;
            struct s2 (* s2) = _heap_obj_7_;
            int n = 2;
            char (* f) = NULL;
    allocation/initialization:
      vars: struct s1 (* s1) = malloc (sizeof (struct s1[1]));
            s1[0].s11 = malloc (sizeof (struct s1[0]));
            s1[0].a = 33686018;
            s1[0].b = 2;
            s1[0].c = malloc (sizeof (char[0]));
            struct s2 (* s2) = malloc (sizeof (struct s2[2]));
            s2[0].s12 = malloc (sizeof (struct s1[0]));
            s2[0].d = 0x1.040404p-123 /*9.55147e-38*/;
            s2[0].e = malloc (sizeof (char[0]));
            s2[1].s12 = malloc (sizeof (struct s1[0]));
            s2[1].d = 0x1.040404p-123 /*9.55147e-38*/;
            s2[1].e = malloc (sizeof (char[0]));
            int n = 2;
            char (* f) = NULL;
    |}]
;;
