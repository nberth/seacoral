(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Basics
open Types

(* --- *)

let pp_access_path = Access_path.print

(* --- *)

let typed_element_string ?(enable_static_attr = true) ?(elt: PPrt.ufmt option)
  : Cil.typ -> string =
  let pp_typ = Cil.printType Cil.defaultCilPrinter () in
  let rec aux t = match t with
    | Cil.TPtr (etyp, attrs) ->
        ptr etyp attrs
    | Cil.TArray (etyp, Some exp, attrs) ->
        array etyp exp attrs
    | _ ->
        fallback t
  and fallback t = match elt with
    | Some pp ->
        Format.asprintf "%s %(%)" (Cil_pretty.sprint ~width:80 (pp_typ t)) pp
    | None ->
        Cil_pretty.sprint ~width:80 (pp_typ t)
  and ptr etyp attrs =
    match Defs.carray_type_attributes attrs with
    | Static_array len
      when enable_static_attr ->
        Format.asprintf "%s[static %d]" (aux etyp) len
    | Static_array len ->
        Format.asprintf "%s[%d]" (aux etyp) len
    | Dynamic_array ->
        fallback (Cil.TPtr (etyp, attrs))
  and array etyp len_exp attrs =
    match Cil.getInteger len_exp with
    | Some len when Cilint.is_int_cilint len ->
        let len = Cilint.int_of_cilint len in
        if enable_static_attr then
          Format.asprintf "%s[static %d]" (aux etyp) len
        else
          Format.asprintf "%s[%d]" (aux etyp) len
    | Some _ | None ->
        fallback (Cil.TArray (etyp, Some len_exp, attrs))
  in
  aux

let string_of_decl: var -> string = fun (t, v, _) ->
  typed_element_string ~enable_static_attr:false ~elt:(PPrt.UFmt.string v) t

let string_of_typ ?enable_static_attr : Cil.typ -> string =
  typed_element_string ?enable_static_attr

(* --- *)

let cil_print_global: Cil.global -> Cil_pretty.doc =
  Cil.printGlobal Cil.defaultCilPrinter ()

let cil_print_exp: Cil.exp -> Cil_pretty.doc =
  Cil.printExp Cil.defaultCilPrinter ()

let cil_print_typ: Cil.typ -> Cil_pretty.doc =
  Cil.printType Cil.defaultCilPrinter ()

let pp_exp ppf exp =
  Fmt.string ppf (Cil_pretty.sprint ~width:80 (cil_print_exp exp))

let pp_typ ppf typ =
  Fmt.string ppf (Cil_pretty.sprint ~width:80 (cil_print_typ typ))

(* --- *)

let is_assignable_typ: Cil.typ -> bool = fun t -> match Cil.unrollType t with
  (* | TVoid _ *)
  | TInt _
  | TFloat _
  | TEnum _
  | TComp _
  | TPtr _ ->
    true
  | _ ->
    false

let strings_of_typ_defs cil =
  List.rev @@ Defs.fold_typ_defs cil begin fun t acc ->
    Cil_pretty.sprint ~width:80 (cil_print_global t) :: acc
  end []

let pp_typed_assignment ppf (t, dest, src) =
  if is_assignable_typ t then
    Fmt.pf ppf "%s = %s" dest src
  else
    let t = string_of_typ ~enable_static_attr:false t in
    Fmt.pf ppf "(void) memcpy (&%s, &%s, sizeof (%s))" dest src t

(* Additional utilities for pretty-printing C declarations *)

let pp_typ_defs ppf cil =
  PPrt.pp_lst
    ~fopen:"" ~fclose:"@\n" ~fsep:"@\n"
    Fmt.string ppf (strings_of_typ_defs cil)

let pp_vars =
  PPrt.pp_lst
    ~fopen:"@[<hov 2>" ~fclose:"@]" ~fsep:",@;"
    (fun ppf d -> Fmt.string ppf (Defs.var_name d))

let pp_formal_decls =
  PPrt.pp_lst
    ~fopen:"@[<hov 2>" ~fclose:"@]" ~fsep:",@;"
    (fun ppf d -> Fmt.string ppf (string_of_decl d))

let pp_decls ?(dpref: PPrt.ufmt = "") =
  PPrt.pp_lst
    ~fempty:""
    ~fopen:"@[<hov>" ~fsep:";@;" ~fclose:";@]"
    (fun ppf d -> Fmt.pf ppf "@[%(%)%s@]" dpref (string_of_decl d))

let pp_global_decls ppf vars
  = pp_decls ppf vars

let pp_argdecl_typs ?enable_static_attr =
  PPrt.pp_lst
    ~fopen:"@[<hv>" ~fsep:",@ " ~fclose:"@]"
    (fun ppf (t, _, _) -> Fmt.string ppf (string_of_typ ?enable_static_attr t))

let pp_func_env ppf { glob_vars; _ } =
  pp_decls ~dpref:"extern@ " ppf glob_vars

let pp_fundecl ?enable_static_attr ppf { func_name; func_rtyp; func_args; _ } =
  Fmt.pf ppf "@[<2>extern@ %s@ %s@ (%a);@]"
    (string_of_typ ?enable_static_attr func_rtyp) func_name
    (pp_argdecl_typs ?enable_static_attr) func_args

(** {2 Generalized pointer access path} *)

let pp_access_path_node ppf = function
  | Access_field s -> Fmt.pf ppf ".%s" s
  | Access_ptr -> Fmt.pf ppf "[_]"

let pp_access_path' ppf =
  List.iter (pp_access_path_node ppf)

let pp_named_location_prefix ppf = function
  | Variable s -> Fmt.string ppf s
  | Struct s -> Fmt.pf ppf "{struct %s}" s

let pp_named_location ppf nl =
  pp_named_location_prefix ppf nl.prefix;
  pp_access_path' ppf nl.access_path

let pp_named_loc_assoc ppf nla =
  match nla with
  | Separate_variables { array; size } ->
    Fmt.pf ppf "%a:%s%a"
      pp_named_location array (fst size)
      pp_access_path' (snd size)
  | From_same_struct { struct_name; array; size } ->
    Fmt.pf ppf "{struct %s}%a:%a"
      struct_name
      pp_access_path' array
      pp_access_path' size

(* --- *)

let emit_testcall ~oracle_assessment ~emit_effective_inputs ~entrypoint
    ?init_func ?oracle_func effective_inputs ppf =
  Option.iter begin fun init ->
    Fmt.pf ppf "(void) %s (%a);@,"
      init.func_name emit_effective_inputs effective_inputs
  end init_func;
  match oracle_func, entrypoint.func_rtyp, entrypoint.func_args with
  | None, _, _ ->
      Fmt.pf ppf "(void) %s (%a);"
        entrypoint.func_name emit_effective_inputs effective_inputs
  | Some oracle, TVoid _, _ ->
      Fmt.pf ppf "(void) %s (%a);@;%s (%s (%a));"
        entrypoint.func_name emit_effective_inputs effective_inputs
        oracle_assessment
        oracle.func_name     emit_effective_inputs effective_inputs
  | Some oracle, _, [] ->
      Fmt.pf ppf "%s (%s (%s (%a)));"
        oracle_assessment
        oracle.func_name
        entrypoint.func_name emit_effective_inputs effective_inputs
  | Some oracle, _, _ ->
      Fmt.pf ppf "%s (%s (@[%a,@;%s (%a)@]));"
        oracle_assessment
        oracle.func_name     emit_effective_inputs effective_inputs
        entrypoint.func_name emit_effective_inputs effective_inputs

(* --- *)

(* Pretty printers for exceptions *)

let () =
  Printexc.register_printer begin function
    | Unknown_function f ->
      PPrt.string_to Option.some "Unknown function `%s'" f
    | Missing_attribute { name; reason } ->
      PPrt.string_to Option.some "Missing attribute `%s' due to %s\
                                 " name reason
    | Incompatible_attributes attrs ->
      PPrt.string_to Option.some "Attributes %a should me mutually exclusive"
        Fmt.(list string) attrs
    | UNSUPPORTED_TYPE BoxedType t ->
      PPrt.string_to Option.some
        "Unsupported@ type@ %s" (Ctypes.string_of_typ t)
    | _ ->
      None
  end
