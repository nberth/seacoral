(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Harness generation *)

open Basics
open Sc_values.TYPES
open Sc_project.Types
open Sc_C.Type_collections.ALIASES                         (* Ctype_{key,map} *)

module AP = Sc_C.Access_path

(* For now, assumes types have already been declared for outputs *)

module TYPES = struct

  type options =
    {
      symbolization_scheme: [`full_struct | `independent_fields];
    }

  type support_data =
    {
      params: any_project_params;
      nonleaf_typs: Ctype_set.t;
      initialized_typs: Ctype_set.t;
      test_struct_type: Ctypes_static.boxed_typ;
      test_struct_symbolization: test_struct_symbolization;
    }
  and test_struct_symbolization =
    | Full of string
    | Independent_fields

  type printer =
    {
      pp_preamble: PPrt.pu;
      pp_body: PPrt.pu;
    }

  and runtime_params =
    {
      symsize_capacity: int;
    }

end
include TYPES

(* --- *)

let support_data (type r) { symbolization_scheme } (params: r project_params) =
  let module Test_repr = (val params.test_repr) in
  let nonleaf_typs
    = Test_repr.reachable_types_with_pointers params.test_struct in
  let test_struct_symbolization =
    match symbolization_scheme with
    | `full_struct -> Full "_inputs"
    | `independent_fields -> Independent_fields
  in
  let initialized_typs, test_struct_type =
    Sc_values.Struct.as_ctyp params.test_struct nonleaf_typs ~f:{
      f = fun t set ->
        let set = match test_struct_symbolization with
          | Full _ -> Ctype_set.add (Ctype_key.of_typ t) set
          | Independent_fields -> set
        in
        set, Ctypes_static.BoxedType t
    }
  in
  {
    params = A params;
    nonleaf_typs;
    initialized_typs;
    test_struct_type;
    test_struct_symbolization;
  }

(* --- *)

let preprocessor_vars (sd: support_data) =
  let module Test_repr = (val Sc_project.Params.any_test_repr sd.params) in
  let { max_non_nil_ptr_depth; max_ptr_array_length; max_cstring_length; _ }
    = Test_repr.Params.encoding_params in
  [
    "__SC_KLEE_MAX_NON_NIL_PTR_DEPTH", Int.to_string max_non_nil_ptr_depth;
    "__SC_KLEE_MAX_PTR_ARRAY_LENGTH", Int.to_string max_ptr_array_length;
    "__SC_KLEE_MAX_CSTRING_LENGTH", Int.to_string max_cstring_length;
  ]

let preprocessor_preamble sd ppf =
  Fmt.(using preprocessor_vars @@
       list (pair (any "#define " ++ string) ~sep:(any " ") string)
         ~sep:(any "@\n") ++ any "@\n") ppf sd

(* ---- *)

let runtime_params ({ params = A params; _ } as sd) =
  let module Test_repr = (val Sc_project.Params.any_test_repr sd.params) in
  let module Encoding = Sc_values.Encoding (Test_repr.Params) in
  {
    symsize_capacity =
      Test_repr.fold_reachable_pointers params.test_struct 1
        ~f:{ f = fun t { pv; _ } -> max (Encoding.max_pointed_size t pv) }
        ~merge:(NEL.fold_left ~f:max 0)
  }

let string_of_ap ap =
  PPrt.to_string "%a" AP.print ap

let make_symbolic ppf (t, v) =
  Fmt.pf ppf "klee_make_symbolic (@[&%s,@ sizeof (%a),@ \"%a\"@]);@;"
    v Sc_values.Printer.c_typ t Sc_values.Printer.c_typ t

let init_fun_name ppf ctype_key =
  Fmt.pf ppf "__sc_init_%s" (Ctype_key.to_string ctype_key)

let init_fun_sign ppf (ctype_key, ptr_name, depth_name) =
  let BoxedType t = Ctype_key.to_boxed_typ ctype_key in
  Fmt.pf ppf "void %a (@[%a,@ const unsigned int %s@])"
    init_fun_name ctype_key
    Sc_values.Printer.c_decl (Ctypes.ptr t, ptr_name) depth_name

let emit_carray_init { nonleaf_typs; _ } ppf (etyp, ap, ub, depth) =
  let ctype_key = Ctype_key.of_typ etyp in
  if Ctype_set.mem ctype_key nonleaf_typs then
    Fmt.pf ppf "__SC_KLEE_INIT_PTR (@[&(%s),@ 0l,@ %ul,@ %s,@ %a,@ %a@]);@]@;"
      (string_of_ap ap) ub depth Sc_values.Printer.c_typ etyp
      init_fun_name ctype_key
  else
    Fmt.pf ppf "__SC_KLEE_INIT_PTR_LEAF (@[&(%s),@ 0l,@ %ul,@ %s,@ %a@]);@]@;"
      (string_of_ap ap) ub depth Sc_values.Printer.c_typ etyp

let emit_constrained_carray_init { nonleaf_typs; _ } ppf (etyp, ap, size_typ,
                                                          size_ap, depth) =
  let ctype_key = Ctype_key.of_typ etyp in
  if Ctype_set.mem ctype_key nonleaf_typs then
    Fmt.pf ppf "__SC_KLEE_INIT_CONSTRAINED_PTR (@[&(%s),@ %a@ &(%s),@ %s,@ \
                %a,@ %a@]);@]@;"
      (string_of_ap ap)
      Sc_values.Printer.c_typ size_typ
      (string_of_ap size_ap)
      depth Sc_values.Printer.c_typ etyp
      init_fun_name ctype_key
  else
    Fmt.pf ppf "__SC_KLEE_INIT_CONSTRAINED_PTR_LEAF (@[&(%s),@ %a,@ &(%s),@ \
                %s,@ %a@]);@]@;"
      (string_of_ap ap)
      Sc_values.Printer.c_typ size_typ
      (string_of_ap size_ap)
      depth Sc_values.Printer.c_typ etyp

let emit_cstring_init ppf (ap, depth) =
  Fmt.pf ppf "__SC_KLEE_INIT_CSTRING (@[&(%s),@ %s@]);@]@;"
    (string_of_ap ap) depth

let declare_support_functions ppf (sd: support_data) =
  let module Test_repr = (val Sc_project.Params.any_test_repr sd.params) in
  let module Encoding = Sc_values.Encoding (Test_repr.Params) in
  Ctype_set.iter begin fun ctype_key ->
    Fmt.pf ppf "@\n%a;" init_fun_sign (ctype_key, "ptr", "depth")
  end sd.initialized_typs;
  Ctype_set.iter begin fun ctype_key ->
    Fmt.pf ppf "@\n@[<2>%a {@;" init_fun_sign (ctype_key, "ptr", "depth");
    let BoxedType t = Ctype_key.to_boxed_typ ctype_key in
    Encoding.ctyp_fold_direct_access_paths t () ~f_ptr:{
      f = fun t ap_suffix pv () ->
        let ap = AP.from "(*ptr)" ap_suffix in
        match pv with
        | `Carray_with_bound_length l ->
            emit_carray_init sd ppf (t, ap, l, "depth")
        | `Carray_with_length_field { ap_suffix; length_field } ->
            let size_ap = AP.append ap ap_suffix in
            let BoxedType size_typ = Sc_values.struct_field_typ length_field in
            emit_constrained_carray_init sd ppf (t, ap, size_typ,
                                                 size_ap, "depth")
        | `Cstring ->
            emit_cstring_init ppf (ap, "depth")
    };
    Fmt.pf ppf "@]@;}"
  end sd.initialized_typs

let symbolize_inputs ppf ({ params = A params; _ } as sd) =
  match sd.test_struct_symbolization with
  | Full test_struct_name ->
      (* Symbolize the inputs structure as a whole *)
      let Ctypes_static.BoxedType t = sd.test_struct_type in
      make_symbolic ppf (t, test_struct_name);
      Fmt.pf ppf "%a (@[&%s,@ 0@]);@;"
        init_fun_name (Ctype_key.of_typ t)
        test_struct_name
  | Independent_fields ->
      (* Symbolize each field in order first so we can easily obtain a
         representation of the struct itself when reading the ktest file. *)
      let symbolize_var t v () =
        make_symbolic ppf (t, v)
      and symbolize_reachable_heap t ap_suffix pv () =
        let ap = AP.HACK.forget_first_suffix_punct @@ Option.get ap_suffix in
        match pv with
        | `Carray_with_bound_length l ->
            emit_carray_init sd ppf (t, ap, l, "0")
        | `Carray_with_length_field { ap_suffix; length_field } ->
            let size_ap = AP.HACK.forget_first_suffix_punct ap_suffix in
            let BoxedType size_typ = Sc_values.struct_field_typ length_field in
            emit_constrained_carray_init sd ppf (t, ap, size_typ, size_ap, "0")
        | `Cstring ->
            emit_cstring_init ppf (ap, "0")
      in
      let module Test_repr = (val Sc_project.Params.any_test_repr sd.params) in
      Sc_values.Struct.fold_fields params.test_struct ()
        ~f:{ f = symbolize_var };
      Test_repr.fold_direct_access_paths params.test_struct ()
        ~f_ptr:{ f = symbolize_reachable_heap }

(* for full inputs struct symbolization *)

let declare_test_struct_type ppf { params = A params; _ } =
  Fmt.pf ppf "@[%a@];@;"
    Sc_values.Struct.definition params.test_struct

let declare_test_struct ppf (sd, test_struct_name) =
  let Ctypes_static.BoxedType t = sd.test_struct_type in
  Fmt.pf ppf "@[%a@];@;" Sc_values.Printer.c_decl (t, test_struct_name)

let env_assign ppf (test_struct_name, Sc_C.Types.{ glob_vars }) =
  PPrt.pp_lst ~fopen:"@[<h>" ~fsep:";@\n" ~fclose:";@]@\n" ~fempty:""
    (fun ppf (t, v, _) ->
       Sc_C.Printer.pp_typed_assignment ppf
         (t, v, Fmt.str "%s.%s" test_struct_name v))
    ppf glob_vars

let emit_effective_inputs ppf (test_struct_name, func_args) =
  PPrt.pp_lst ~fopen:"@[<hv>" ~fsep:",@\n" ~fclose:"@]"
    (fun ppf v -> Fmt.pf ppf "%s.%s" test_struct_name (Sc_C.Defs.var_name v))
    ppf func_args

(* for field-independent symbolization *)

let declare_tested_function_args ppf { params = A params; _ } =
  (* Declare local variables for function arguments: we use the type of fields
     for this purpose so assignments are consistent w.r.t the inputs
     structure. *)
  let args = Sc_C.Defs.varset params.func_repr.func_args in
  let declare_arg typ v () =
    if Strings.mem v args
    then Fmt.pf ppf "%a;@;" Sc_values.Printer.c_decl (typ, v)
  in
  Sc_values.Struct.fold_fields params.test_struct ()
    ~f:{ f = declare_arg }

(*  *)

let entrypoint _ = "main"

let body ({ params = A params; _ } as sd) ppf =
  match sd.test_struct_symbolization with
  | Full test_struct_name ->
      Fmt.pf ppf
        "#include <string.h> /* for `memcpy` */\
         @\n@[<v>%a@]\
         @\n@[<v>%a@]\
         @\nint %s () {\
         @\n  @[<v>%a@]\
         @\n  @[<v>%a@]\
         @\n  @[<v>%a@]\
         @\n  @[<v>%t@]\
         @\n  __SC_KLEE_EXIT ();\
         @\n  return 0;\
         @\n}\
         @."
        declare_test_struct_type sd
        declare_support_functions sd
        (entrypoint sd)
        declare_test_struct (sd, test_struct_name)
        symbolize_inputs sd
        env_assign (test_struct_name, params.func_repr.func_env)
        (Sc_project.Printer.C.emit_testcall params
           ~oracle_assessment:"klee_assert"
           ~emit_effective_inputs (test_struct_name,
                                   params.func_repr.func_args))
  | Independent_fields ->
      Fmt.pf ppf
        "@[<v>%a@]\
         @\nint %s () {\
         @\n  @[<v>%a@]\
         @\n  @[<v>%a@]\
         @\n  @[<v>%t@]\
         @\n  __SC_KLEE_EXIT ();\
         @\n  return 0;\
         @\n}\
         @."
        declare_support_functions sd
        (entrypoint sd)
        declare_tested_function_args sd
        symbolize_inputs sd
        (Sc_project.Printer.C.emit_testcall params
           ~oracle_assessment:"klee_assert"
           ~emit_effective_inputs:Sc_C.Printer.pp_vars
           params.func_repr.func_args)

let printer sd =
  {
    pp_preamble = preprocessor_preamble sd;
    pp_body = body sd;
  }
