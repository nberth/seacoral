(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Sc_sys.File.TYPES
open Sc_C.Type_collections.ALIASES                         (* Ctype_{key,map} *)

module AP = Sc_C.Access_path

let install_resources_in: workspace:_ -> dir Lwt.t =
  Sc_core.Workspace.install_resources_in @@
  Sc_core.Resource.register_crunched "raw-decoder" (module Res)

let declare_struct ppf inputs_struct =
  Cvalues.Struct.definition ppf inputs_struct

let emit_struct_ptr_typ ppf inputs_struct =
  Fmt.pf ppf "%a *" Cvalues.Struct.print inputs_struct     (* ok for `struct` *)

let emit_init_pointer_fields_symbol ppf t =
  Fmt.pf ppf "__sc_decode_%a" Ctype_key.print (Ctype_key.of_typ t)

let emit_ptr_decoder_symbol ppf t =
  Fmt.pf ppf "__sc_alloc_%a" Ctype_key.print (Ctype_key.of_typ t)

let emit_struct_decoder_symbol ppf inputs_struct =
  Cvalues.Struct.as_ctyp ~f:{ f = fun t () -> emit_ptr_decoder_symbol ppf t }
    inputs_struct ()

let emit_init_pointer_fields_symbol_addr ppf t =
  if Cvalues.has_pointers t
  then Fmt.pf ppf "&%a" emit_init_pointer_fields_symbol t
  else Fmt.string ppf "NULL"

let emit_decode_cstring ppf ap =
  Fmt.pf ppf "__SC_RAW_DECODE_CSTRING (%a);@;"
    AP.print ap

let emit_decode_carray ppf (t, ap, ub) =
  Fmt.pf ppf "__SC_RAW_DECODE_ARRAY (@[%a,@ %a,@ %a,@ %u,@ %u,@ %a@]);@;"
    Cvalues.Printer.c_typ t
    Cvalues.Printer.c_typ (Pointer t)
    AP.print ap
    0 ub
    emit_init_pointer_fields_symbol_addr t

let emit_decode_constrained_carray ppf (t, ap, ub, size_typ, size_ap) =
  Fmt.pf ppf "__SC_RAW_DECODE_CONSTRAINED_ARRAY (@[%a, %a, %a, %u, %a, \
              &(%a), %a@]);@;"
    Cvalues.Printer.c_typ t
    Cvalues.Printer.c_typ (Pointer t)
    AP.print ap
    ub
    Cvalues.Printer.c_typ size_typ
    AP.print size_ap
    emit_init_pointer_fields_symbol_addr t

let emit_struct_decoder ppf ((module Raw_test: Cvalues.Struct.REPR), inputs_struct) =
  let module Encoding = Cvalues.Encoding (Raw_test.Params) in
  let nonleaf_typs
    = Raw_test.reachable_types_with_pointers inputs_struct in
  let pp_alloc_sign ppf t =
    Fmt.pf ppf "\
           static void*\
        @\n%a (@[<hov>__sc_buff_t * const buff,@ %a@])"
      emit_ptr_decoder_symbol t
      Cvalues.Printer.c_decl (Primitive Uint, "const depth")
  and pp_init_sign ppf t =
    Fmt.pf ppf "\
           static void*\
        @\n%a (@[<hov>__sc_buff_t * const buff,@ %a,@ %a@])"
      emit_init_pointer_fields_symbol t
      Cvalues.Printer.c_decl (Primitive Uint, "const depth")
      Cvalues.Printer.c_decl (Pointer Void, "const pp")
  in
  let pp_init_impl ppf t =
    pp_init_sign ppf t;
    Fmt.pf ppf
      "@ {\
       @\n  %a;\
       @\n  p = (%a) pp;\
       @\n  @[<v>"
      Cvalues.Printer.c_decl (Pointer t, "p")
      Cvalues.Printer.c_typ (Pointer t);
    Encoding.ctyp_fold_direct_access_paths t () ~f_ptr:{
      f = fun t ap_suffix pv () ->
        let ap = AP.from "(*p)" ap_suffix in
        match pv with
        | `Cstring ->
            emit_decode_cstring ppf ap
        | `Carray_with_bound_length ub ->
            emit_decode_carray ppf (t, ap, ub)
        | `Carray_with_length_field { ap_suffix; length_field } ->
            let ub = Raw_test.Params.encoding_params.max_ptr_array_length in
            let BoxedType size_typ = Cvalues.struct_field_typ length_field in
            let size_ap = AP.from "(*p)" (Some ap_suffix) in
            emit_decode_constrained_carray ppf (t, ap, ub, size_typ, size_ap)
    };
    Fmt.pf ppf "\
          @]return p;\
        @\n}\
        @\n"
  in
  let pp_alloc_impl ppf t =
    Fmt.pf ppf "\
        %a@ {@\n\
        @[<2>  %a;\
        @\nconst long size = sizeof (%a);\
        @\nif ((buff->remaining_bytes -= size) < 0l) return NULL;\
        @\np = (%a) __sc_malloc_n_copy (buff, size);\
        @\nreturn %a (buff, depth, p);@]\
        @\n}"
      pp_alloc_sign t
      Cvalues.Printer.c_decl (Pointer t, "p")
      Cvalues.Printer.c_typ t
      Cvalues.Printer.c_typ (Pointer t)
      emit_init_pointer_fields_symbol t
  in
  (* decrease depth by one to account for root inputs structure *)
  let depth_expr = "(((depth) > 0u) ? ((depth) - 1u) : 0u)" in
  Fmt.pf ppf "#define __SC_PTR_DECODE(ptr_code, lb, ub, depth) \\\
              @\n\t@[<h>%a@]@\n@\n"
    Encoding.pp_c_ptr_decode ("ptr_code", "lb", "ub", depth_expr);
  Fmt.pf ppf "#define __SC_CONSTRAINED_CARRAY_LENGTH(ptr_code, size, \
              ub, depth) \\\
              @\n\t@[<h>%a@]@\n@\n"
    Encoding.pp_c_constrained_carray_length ("ptr_code", "size",
                                             "ub", depth_expr);
  Fmt.pf ppf "#define __SC_CSTRING_DECODE(ptr_code, depth) \\\
              @\n\t@[<h>%a@]@\n@\n"
    Encoding.pp_c_cstring_buff_length_decode ("ptr_code", depth_expr);
  Fmt.pf ppf "#include \"sc-raw-decoder.c\"@\n";
  Fmt.pf ppf "#include \"sc-raw-decoder-macros.h\"@\n@\n";
  Ctype_set.iter begin fun k ->
    let BoxedType t = Ctype_key.to_boxed_typ k in
    Fmt.pf ppf "%a;@\n" pp_init_sign t;
  end nonleaf_typs;
  Fmt.pf ppf "@\n";
  Ctype_set.iter begin fun k ->
    let BoxedType t = Ctype_key.to_boxed_typ k in
    Fmt.pf ppf "%a@\n" pp_init_impl t;
  end nonleaf_typs;
  Cvalues.Struct.as_ctyp inputs_struct ppf ~f:{
    f = fun t ppf ->
      pp_init_impl ppf t;
      Fmt.pf ppf "@\n";
      pp_alloc_impl ppf t
  }
