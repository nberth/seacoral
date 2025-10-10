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

module Log = (val (Ez_logs.from_src @@ Logs.Src.create "Sc_C.Defs"))

let var_name: var -> string = fun (_, v, _) -> v
let var_names: vars -> string list = List.map var_name
let var_names': vars -> string list = List.rev_map var_name
let varset: vars -> Strings.t = fun vars -> Strings.of_list @@ var_names' vars

let typ : var -> Cil.typ = fun (t, _, _) -> t

let size_arg = function
  | `Int i -> [Cil.AInt i]                                            (* const *)
  | `Var v -> [Cil.AStr v]

let rec remove_attribute name = function
  | [] -> []
  | Cil.Attr (n, _) :: tl when n = name -> remove_attribute name tl
  | a :: tl -> a :: remove_attribute name tl                (* non-tailrec, fine *)

let as_pointer_to_carray ?size (t, v, a) =
  let args = match size with Some sz -> size_arg sz | None -> [] in
  t, v, Cil.(Attr ("carray", args)) :: remove_attribute "carray" a

let as_pointer_to_cstring (t, v, a) =
  t, v, Cil.(Attr ("cstring", [])) :: remove_attribute "cstring" a

let find_attribute attr_name =
  List.find_map begin function
    | Cil.Attr (tag, params) when tag = attr_name -> Some params
    | _ -> None
  end

let interpret_attribute ~f name attrs =
  find_attribute name attrs |>
  Option.map begin fun payload ->
    try f payload
    with Exit -> raise @@ Invalid_attribute_payload (Attr (name, payload))
  end

let unit_attribute name attrs : bool =                     (* checks presence *)
  interpret_attribute name attrs ~f:(function [] -> () | _ -> raise Exit) |>
  Option.fold ~none:false ~some:(fun () -> true)

let arraylen_attribute attrs =
  interpret_attribute "arraylen" attrs ~f:begin function
    | [Cil.AInt len] -> len
    | _ -> raise Exit
  end

let carray_attribute attrs =
  interpret_attribute "carray" attrs ~f:begin function
    | [] -> `Carray_with_unknown_length
    | [Cil.AInt i] -> `Carray_with_bound_length i
    | [Cil.AStr s] -> `Carray_with_length_field s
    | _ -> raise Exit
  end

let cstring_attribute attrs =
  interpret_attribute "cstring" attrs ~f:begin function
    | [] | [Cil.AInt _ | AStr _] -> `Cstring
    | _ -> raise Exit
  end

let static_attribute attrs =
  unit_attribute "static" attrs

let const_attribute attrs =
  unit_attribute "const" attrs

let pointer_validity attrs =
  match carray_attribute attrs, cstring_attribute attrs with
  | None, None ->
      `Carray_with_bound_length 1                 (* naked pointer by default *)
  | Some a, None | None, Some a ->
      a
  | Some _, Some _ ->
      raise @@ Incompatible_attributes ["carray"; "cstring"]

(* --- *)

let typ_attributes: Cil.typ -> Cil.attributes = fun typ ->
  match Cil.unrollType typ with
  | TVoid a
  | TInt (_, a)
  | TFloat (_, a)
  | TPtr (_, a)
  | TArray (_, _, a)
  | TFun (_, _, _, a)
  | TNamed (_, a)
  | TComp (_, a)
  | TEnum (_, a)
  | TBuiltin_va_list a -> a

let const_typ: Cil.typ -> bool = fun typ ->
  const_attribute @@ typ_attributes typ

(* --- *)

let carray_type_attributes attrs : carray_type_attrs =
  let len = arraylen_attribute attrs
  and static = static_attribute attrs in
  match len with
  | Some len when static ->
      Static_array len
  | None when static ->
      raise @@ Missing_attribute { name = "arraylen"; reason = "static array" }
  | None | Some _ ->
      Dynamic_array

let get_fun (fname : string) (gl : Cil.global list) : Cil.varinfo =
  let rec loop = function
    | [] ->
        raise (Unknown_function fname)
    | Cil.GFun (Cil.{svar; _}, _) :: _
      when svar.vname = fname ->
        svar
    | _ :: tl ->
        loop tl
  in
  loop gl

let map_filter_globals f cil =
  Cil.foldGlobals cil begin fun acc g ->
    (Option.fold ~none:Fun.id ~some:List.cons (f g)) acc
  end []

let fold_typ_defs cil f =
  Cil.foldGlobals cil begin fun acc -> function
    | Cil.GType _
    | GCompTag _ | GCompTagDecl _
    | GEnumTag _ | GEnumTagDecl _ as t -> f t acc
    | _ -> acc
  end

(** [diff_globals cil1 cil2] Performs a diff between [cil1] and [cil2] by
    relying on source file locations to check equality.  *)
let diff_globals =
  let open Cil in
  let module CilLoc = struct
    type t = location
    let compare
        { line = l1; column = c1; file = f1; _ }
        { line = l2; column = c2; file = f2; _ } =
      Stdlib.compare (l1, c1, f1) (l2, c2, f2)
  end in
  let open Stdlib.Set.Make (CilLoc) in
  let module Acc = struct
    type nonrec t = { typs: t; funs: t; vars: t; }
    let empty = { typs = empty; funs = empty; vars = empty; }
  end in
  let names cil =
    let open Acc in
    foldGlobals cil begin fun acc -> function
      | Cil.GType (_, t)
      | GCompTag (_, t) | GCompTagDecl (_, t)
      | GEnumTag (_, t) | GEnumTagDecl (_, t) ->
          { acc with typs = add t acc.typs }
      | GFun (_, f) ->
          { acc with funs = add f acc.funs }
      | GVar (_, _, v) | GVarDecl (_, v) ->
          { acc with vars = add v acc.vars }
      | _ -> acc
    end Acc.empty
  in
  fun c1 c2 ->
    let Acc.{ typs = t2; funs = f2; vars = v2 } = names c2 in
    {
      c1 with
      globals = List.rev @@ map_filter_globals begin function
          | GType (_, t)
          | GCompTag (_, t) | GCompTagDecl (_, t)
          | GEnumTag (_, t) | GEnumTagDecl (_, t) when mem t t2 -> None
          | GFun (_, f) when mem f f2 -> None
          | GVar (_, _, v) | GVarDecl (_, v) when mem v v2 -> None
          | g -> Some g
        end c1
    }

type _ arg_info =
  | Typ: Cil.typ arg_info
  | UTyp: Cil.typ arg_info
  | Var: string arg_info
  | TypNVar: var arg_info
  | UTypNVar: var arg_info

exception ARG_INFO_ERROR of Cil.typ

let get_args_infos (type k) (i: k arg_info) (t : Cil.typ) : k list =
  let select: (string * Cil.typ * _) -> k = fun (s, t, l) -> match i with
    | Typ -> t
    | UTyp -> Cil.unrollType t
    | Var -> s
    | TypNVar -> t, s, l
    | UTypNVar -> Cil.unrollType t, s, l
  in
  try
    let (_,args, _, _) = Cil.splitFunctionType t in
    let args = Option.value ~default:[] args in
    (* Maybe a bug: no argument = declaration? *)
    List.map select args
  with GoblintCil.Errormsg.Error ->
    (* Todo: replace this error message *)
    raise @@ ARG_INFO_ERROR t
(* failwith ("Get Args Type ERROR with type " ^ string_of_typ t) *)


let get_arg_names = get_args_infos Var
let get_arg_typs = get_args_infos Typ
let get_arg_unrolled_typs = get_args_infos UTyp
let get_args = get_args_infos TypNVar
let get_unrolled_args = get_args_infos UTypNVar

let fun_ f fname cil = f (get_fun fname cil.Cil.globals).vtype
let fun_arg_names = fun_ get_arg_names
let fun_arg_typs = fun_ get_arg_typs
let fun_arg_unrolled_typs = fun_ get_arg_unrolled_typs
let fun_args = fun_ get_args
let fun_unrolled_args = fun_ get_unrolled_args

let fun_return_typ fname cil =
  let vinf = get_fun fname cil.Cil.globals in
  let rtyp, _, _, _ = Cil.splitFunctionTypeVI vinf in
  rtyp

let get_var_info (type k) (i : k arg_info) (v : Cil.varinfo) : k =
  match i with
  | Typ -> v.vtype
  | UTyp -> Cil.unrollType v.vtype
  | Var -> v.vname
  | TypNVar -> v.vtype, v.vname, v.vattr
  | UTypNVar -> Cil.unrollType v.vtype, v.vname, v.vattr

let map_filter_glob_vars f =
  let is_fun v = match v.Cil.vtype with TFun _ -> true | _ -> false in
  map_filter_globals begin function
    | Cil.GVarDecl (v, _) | GVar (v, {init = None}, _) when not (is_fun v) ->
        Some (f v)
    | _ ->
        None
  end
let vars t = map_filter_glob_vars (get_var_info t)
let glob_types = vars Typ
let glob_unrolled_types = vars UTyp
let glob_names = vars Var
let glob_infos = vars TypNVar
let glob_unrolled_infos = vars UTypNVar

let func_env ?for_func cil =
  ignore for_func;
  {                                  (* use all declared globals (for now?) *)
    glob_vars = glob_unrolled_infos cil;
  }

let func_repr cil func_env func_name =
  {
    func_name;
    func_env;
    func_rtyp = fun_return_typ func_name cil;
    func_args = fun_unrolled_args func_name cil;
  }

(* TODO: handle shadowing of global variables by function arguments *)
let func_inputs { func_args; func_env = { glob_vars }; _ } =
  func_args @ glob_vars

let map_func_inputs ~f ({ func_args; func_env = { glob_vars }; _ } as func) =
  { func with
    func_args = List.map (f Formal) func_args;
    func_env = { glob_vars = List.map (f Global) glob_vars; }; }

let cil_func cil f =
  let func_env = func_env ~for_func:f cil in
  func_repr cil func_env f

let if_array ty : (carray_type_attrs * Cil.typ) option =
  match ty with
  | Cil.TPtr (etyp, l) ->
      Some (carray_type_attributes l, etyp)
  | Cil.TArray (etyp, None, _) ->
      Some (Dynamic_array (* `Unknown *), etyp)
  | Cil.TArray (etyp, Some exp, _) ->
      (match Cil.getInteger exp with
       | Some array_len when Cilint.is_int_cilint array_len ->
           let array_len = Cilint.int_of_cilint array_len in
           Some (Static_array array_len, etyp)
       | None | Some _ -> Some (Dynamic_array (* `Unknown *), etyp))
  | _ ->
      None

let multidim_array_sizes ty =
  let rec loop acc ty =
    match if_array ty with
    | None -> List.rev acc
    | Some (size, ty) -> loop (size :: acc) ty
  in
  loop [] ty

let parse_file f =
  let c_file = Sc_sys.File.name f in
  try
    GoblintCil.Frontc.parse c_file ()
  with GoblintCil.Frontc.ParseError msg ->
    Fmt.failwith "Error while parsing `%s': %s" c_file msg
