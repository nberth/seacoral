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
open Sc_C.Type_collections.ALIASES

(* --- *)

module TYPES = struct

  type cside
  type enum_ = [`Enum]
  type struct_ = [`Struct]
  type union_ = [`Union]
  type array_ = [`Array]
  type named_mapping_ = [struct_|union_]
  type aggreg_ = [named_mapping_|array_]
  type any_ = [enum_|aggreg_]
  type _ ctyp =
    | CEnum: (string, 'a) Ctypes_static.view -> [>enum_] ctyp
    | CStruct: cside Ctypes_static.structure_type -> [>struct_] ctyp
    | CUnion: cside Ctypes_static.union_type -> [>union_] ctyp
    | CArray: 'a Ctypes.typ * int -> [>array_] ctyp

  type 'a ctyp_consumer = { f: 'k. 'k Ctypes.typ -> 'a }

  type 'a defs = 'a ctyp StrMap.t
  type typdecls = any_ defs

  type encoding_params =
    {
      max_ptr_array_length: int;
      max_non_nil_ptr_depth: int;
      max_cstring_length: int;                 (* excluding a trailing '\000' *)
    }

  type ptr_attributes =
    {
      pv: memory_validity;
      depth: int;
    }

  and memory_validity =
    [ `Carray_with_bound_length of int
    | `Carray_with_length_field of field_access
    | `Cstring ]

  and field_access =
    {
      ap_suffix: Sc_C.Types.access_path_suffix;
      length_field: struct_field;
    }

  and struct_field =
    | Struct_field:
        {
          struct_spec: 's Ctypes_static.structure_type;
          field: ('a, ('s, [`Struct]) Ctypes.structured) Ctypes.field;
          field_typ: 'a integral_typ;
        } -> struct_field
  and _ integral_typ =
    | Char_integral_typ: char integral_typ
    | Schar_integral_typ: int integral_typ
    | Uchar_integral_typ: Unsigned.uchar integral_typ
    | Short_integral_typ: int integral_typ
    | Int_integral_typ: int integral_typ
    | Long_integral_typ: Signed.long integral_typ
    | Llong_integral_typ: Signed.llong integral_typ
    | Ushort_integral_typ: Unsigned.ushort integral_typ
    | Sint_integral_typ: Signed.sint integral_typ
    | Uint_integral_typ: Unsigned.uint integral_typ
    | Ulong_integral_typ: Unsigned.ulong integral_typ
    | Ullong_integral_typ: Unsigned.ullong integral_typ
    | Size_t_integral_typ: Unsigned.size_t integral_typ
    | Int8_t_integral_typ: int integral_typ
    | Int16_t_integral_typ: int integral_typ
    | Int32_t_integral_typ: int32 integral_typ
    | Int64_t_integral_typ: int64 integral_typ
    | Uint8_t_integral_typ: Unsigned.uint8 integral_typ
    | Uint16_t_integral_typ: Unsigned.uint16 integral_typ
    | Uint32_t_integral_typ: Unsigned.uint32 integral_typ
    | Uint64_t_integral_typ: Unsigned.uint64 integral_typ

  type heap_block =
    | Heap_block:
        's Ctypes_static.structure_type
        * 's Ctypes.structure Ctypes.ptr
        -> heap_block

  type 'a ptr_attributes_folder =
    (ptr_attributes -> 'a -> 'a) ctyp_consumer

  type 'a access_path_folder =
    (Sc_C.Types.access_path_suffix option -> 'a -> 'a) ctyp_consumer

  type 'a ptr_access_path_folder =
    (Sc_C.Types.access_path_suffix option -> memory_validity ->
     'a -> 'a) ctyp_consumer

  type 'a field_folder =
    (string -> 'a -> 'a) ctyp_consumer

  type 'a ptr_field_folder =
    (string -> memory_validity -> 'a -> 'a) ctyp_consumer

  type 'a visit_action =
    | Do_children of 'a
    | Do_children_and_then of 'a * ('a -> 'a)
    | Skip_children of 'a

  type 'a access_path_visitor =
    (Sc_C.Types.access_path_suffix option -> 'a -> 'a visit_action) ctyp_consumer

  type 'a ptr_access_path_visitor =
    (Sc_C.Types.access_path_suffix option -> memory_validity ->
     'a -> 'a visit_action) ctyp_consumer

  type 'a cptr_consumer =
    {
      p: 'k. 'k Ctypes.typ -> 'k Ctypes.ptr -> ?len:int -> ptr_attributes -> 'a;
    }

  type specification_error =
    | Unknown_field of
        { field_name: string; aggreg_type_tag: string }
    | Invalid_length_field of
        { field_name: string; field_typ: Ctypes_static.boxed_typ }
    | Unsupported_type of Cil.typ
    | Unknown_enum of Cil.enuminfo
    | Unknown_comp of Cil.compinfo

  exception SPECIFICATION_ERROR of specification_error

  type literal_binding =
    | LBStr of string
    | LBMap of literal_binding StrMap.t
    | LBArr of literal_binding array
    | LBLst of literal_binding list
    | LBNil
    | LBRef of literal_binding

  type literal_binding_error =
    | Unexpected_literal of
        {
          binding: literal_binding;
          assigned: (string * Ctypes_static.boxed_typ) option;
          msg: string;
        }
    | Unsupported_assignment of
        assignment_info
    | Unknown_enum of
        {
          enumname: string;
          assignment: assignment_info;
        }
    | Incompatible_array_length of
        {
          expected_len: int;
          rvalue: literal_binding array;
        }
    | Bad_array_literal_length of
        {
          max_len: int;
          given_len: int;
          assignment: assignment_info;
        }
    | Bad_cstring_literal_length of
        {
          string: string;
          params: encoding_params;
          assignment: assignment_info;
        }

  and assignment_info =
    {
      binding: literal_binding;
      assigned: string * Ctypes_static.boxed_typ;
    }

  exception LITERAL_BINDING_ERROR of literal_binding_error

end
include TYPES

(** Abstract field representation *)
module Field = struct

  (** Singleton GADT constructor; unboxed to use a reference to the parametric
      field itself *)
  type t = F: (_ (* Ctypes_static.ptr *), _) Ctypes_static.field -> t [@@unboxed]

  (** Hash on field name *)
  let hash (F { fname; _ }) = Hashtbl.hash fname

  (** Use physical equality on field types *)
  let equal = (==)

end

(** Type of weak hashtable to associate any structure field with attributes. *)
module FieldH = Ephemeron.K1.Make (Field)

type elaborated_pointer_validity =
  [ memory_validity
  | `Carray_with_unknown_length ]

(** Associates pointer fields with validity attributes *)
let ptr_field_attrs: elaborated_pointer_validity FieldH.t =
  FieldH.create 9

let ptr_field_validity f =
  try FieldH.find ptr_field_attrs (F f)
  with Not_found -> `Carray_with_unknown_length

let single_cell_validity =
  `Carray_with_bound_length 1

(* --- *)

let typ_size = function
  | CEnum view -> Ctypes.sizeof (View view)
  | CStruct s -> Ctypes.sizeof (Struct s)
  | CUnion u -> Ctypes.sizeof (Union u)
  | CArray (typ, len) -> Ctypes.sizeof (Array (typ, len))

let print_def ppf = function
  | CEnum view -> Ctypes.format_typ ppf (View view)
  | CStruct s -> Ctypes.format_typ ppf (Struct s)
  | CUnion u -> Ctypes.format_typ ppf (Union u)
  | CArray (typ, len) -> Ctypes.format_typ ppf (Array (typ, len))

let format_typ' ppf t =
  Ctypes.format_typ ?name:None ppf t

let print_typ ppf = function
  | CEnum _ as t -> print_def ppf t
  | CStruct { tag; _ } -> Fmt.pf ppf "struct %s" tag
  | CUnion { utag; _ } -> Fmt.pf ppf "union %s" utag
  | CArray _ as t -> print_def ppf t

let as_ctyp t ~f:{ f } = match t with
  | CEnum v -> f (View v)
  | CStruct s -> f (Struct s)
  | CUnion u -> f (Union u)
  | CArray (typ, len) -> f (Array (typ, len))

(* --- Error printers --- *)

let specification_error e =
  raise @@ SPECIFICATION_ERROR e

let pp_specification_error ppf = function
  | Unknown_field { field_name; aggreg_type_tag } ->
      Fmt.pf ppf "Field@ `%s'@ not@ found@ in@ struct@ or@ union@ %s\
                 " field_name aggreg_type_tag
  | Invalid_length_field { field_name; field_typ = BoxedType typ } ->
      Fmt.pf ppf "Unexpected@ type@ %a@ for@ field@ `%s'@ (integral@ type@ \
                  expected)" format_typ' typ field_name
  | Unsupported_type typ ->
      Fmt.pf ppf "Unsupported@ type@ %a" Sc_C.Printer.pp_typ typ
  | Unknown_enum enuminfo ->
      Fmt.pf ppf "Unknown@ type@ %a" Sc_C.Printer.pp_typ (TEnum (enuminfo, []))
  | Unknown_comp compinfo ->
      Fmt.pf ppf "Unknown@ type@ %a" Sc_C.Printer.pp_typ (TComp (compinfo, []))

(* --- *)

let empty_typdecls: typdecls = StrMap.empty

let declare_typ: string -> any_ ctyp -> typdecls -> typdecls = StrMap.add
let find_typ: string -> typdecls -> any_ ctyp = StrMap.find

let stdbool =
  let show_stdbool v =
    Unsigned.UChar.(if v = zero then "false"
                    else if v = one then "true"
                    else to_string v)
  in
  Ctypes.view Ctypes.uchar
    ~read:Fun.id ~write:Fun.id
    ~format_typ:(fun pp_var ppf -> Fmt.pf ppf "_Bool@,%t" pp_var)
    ~format:(fun ppf v -> Fmt.string ppf (show_stdbool v))

(* A boxed type that still allows to check physical equality *)
type boxed_typ = Boxed_typ: _ Ctypes.typ -> boxed_typ                 [@@unboxed]

let ctyp: typdecls -> 'a ctyp_consumer -> Cil.typ -> 'a = fun defs ->
  let unsupported t = specification_error @@ Unsupported_type t in
  let open Ctypes in
  let rec ctyp ({ f } as c : 'a ctyp_consumer) t : 'a =
    match Cil.unrollType t with
    | TVoid _ -> f void
    | TInt (IBool, _) -> f stdbool
    | TInt ((IChar|ISChar), _) -> f char
    | TInt (IUChar, _) -> f uchar
    | TInt (IShort, _) -> f short
    | TInt (IUShort, _) -> f ushort
    | TInt (IInt, _) -> f int
    | TInt (IUInt, _) -> f uint
    | TInt (ILong, _) -> f long
    | TInt (IULong, _) -> f ulong
    | TInt (ILongLong, _) -> f llong
    | TInt (IULongLong, _) -> f ullong
    | TInt ((IInt128 | IUInt128), _) as t -> unsupported t
    | TFloat (FFloat, _) -> f float
    | TFloat (FDouble, _) -> f double
    | TFloat (FLongDouble, _) -> f ldouble
    | TFloat (FComplexFloat, _) -> f complex32
    | TFloat (FComplexDouble, _) -> f complex64
    | TFloat (FComplexLongDouble, _) -> f complexld
    | TFloat ((FFloat128 | FComplexFloat128), _) as t -> unsupported t
    | TEnum (enuminfo, _) -> enum c enuminfo
    | TComp (compinfo, _) -> comp c compinfo
    | TPtr (TFun (ret_typ, None, _, _), _)
    |       TFun (ret_typ, None, _, _)     ->               (* CHECKME: equiv? *)
        ctyp { f = fun t -> f @@ static_funptr @@ returning t} ret_typ
    | TPtr (TFun (ret_typ, Some [], _, _), _) ->
        ctyp { f = fun t -> f @@ static_funptr @@ void @-> returning t} ret_typ
    | TPtr (TFun (ret_typ, Some [_, arg, _], _, _), _) ->
        (* This case is redundant with the next one; it however is a good example
           of how the general case works, so we kept it. *)
        ctyp { f = fun t ->
            ctyp { f = fun targ ->
                f @@ static_funptr @@ targ @-> returning t
              } arg
          } ret_typ
    | TPtr (TFun (ret_typ, Some args, _, _), _)
    |       TFun (ret_typ, Some args, _, _)     ->          (* CHECKME: equiv? *)
        let rec rev_args: type r. r fn -> _ -> 'a = fun fn -> function
          | [] ->
              f @@ static_funptr @@ fn
          | (_, arg_typ, _) :: more_args ->
              ctyp { f = fun t -> rev_args (t @-> fn) more_args } arg_typ
        in
        ctyp { f = fun t -> rev_args (returning t) (List.rev args) } ret_typ
    | TPtr (etyp, attributes) ->
        basic_ptr c etyp attributes
    | TArray (etyp, Some exp, attributes) ->
        carray c etyp exp attributes
    | TArray (_, None, _)
    | TBuiltin_va_list _
    | TNamed _ as t ->
        unsupported t
  and enum { f } ({ ename; _ } as enuminfo) =
    match find_typ ename defs with
    | CEnum view ->
        f (View view)
    | _ ->
        Fmt.failwith "Invalid internal CIL type for enumeratiom %s" ename
    | exception Not_found ->
        specification_error @@ Unknown_enum enuminfo
  and comp c ({ cname; _ } as compinfo) =
    try
      as_ctyp ~f:c @@ find_typ cname defs
    with Not_found ->
      specification_error @@ Unknown_comp compinfo
  and basic_ptr { f } etyp attributes =
    try match Sc_C.Defs.carray_type_attributes attributes with
      | Static_array len ->
          ctyp { f = fun t -> f (array len t) } etyp
      | Dynamic_array ->
          ctyp { f = fun t -> f (ptr t) } etyp
    with
    | SPECIFICATION_ERROR Unknown_comp { cname = name; _ }
    | SPECIFICATION_ERROR Unknown_enum { ename = name; _ } ->
        Logs.warn (fun p -> p "Unknown referenced type %s: using void" name);
        f (ptr void)
  and carray { f } etyp length attributes =
    match Cil.getInteger (Cil.constFold true length) with
    | Some len when Cilint.is_int_cilint len ->
        let len = Cilint.int_of_cilint len in
        ctyp { f = fun t -> f (array len t) } etyp
    | _ ->
        unsupported @@ TArray (etyp, Some length, attributes)
  in
  ctyp

let struct_field_typ (Struct_field { field; _ }) : Ctypes_static.boxed_typ =
  Ctypes_static.BoxedType field.ftype

let struct_field_as_length: blk:heap_block -> struct_field -> int64 * (int -> unit) =
  (* TODO: check invalid block type (seen as invalid coercion) *)
  fun ~blk:(Heap_block (bspec, bptr))
    (Struct_field { struct_spec; field = f; field_typ }) ->
    let open Ctypes in
    let open Signed in
    let open Unsigned in
    let b = coerce (ptr @@ Struct bspec) (ptr @@ Struct struct_spec) bptr in
    let p = b |-> f in
    let assign f = fun i -> p <-@ f i in
    match field_typ with
    | Char_integral_typ     -> Int64.of_int @@ Char.code !@p, assign Char.chr
    | Schar_integral_typ    -> Int.to_int64 !@p, (<-@) p
    | Short_integral_typ    -> Int.to_int64 !@p, (<-@) p
    | Int_integral_typ      -> Int.to_int64 !@p, (<-@) p
    | Int8_t_integral_typ   -> Int.to_int64 !@p, (<-@) p
    | Int16_t_integral_typ  -> Int.to_int64 !@p, (<-@) p
    | Uchar_integral_typ    -> UChar.to_int64  !@p, assign UChar.of_int
    | Long_integral_typ     -> Long.to_int64   !@p, assign Long.of_int
    | Llong_integral_typ    -> LLong.to_int64  !@p, assign LLong.of_int
    | Ushort_integral_typ   -> UShort.to_int64 !@p, assign UShort.of_int
    | Sint_integral_typ     -> SInt.to_int64   !@p, assign SInt.of_int
    | Uint_integral_typ     -> UInt.to_int64   !@p, assign UInt.of_int
    | Ulong_integral_typ    -> ULong.to_int64  !@p, assign ULong.of_int
    | Ullong_integral_typ   -> ULLong.to_int64 !@p, assign ULLong.of_int
    | Size_t_integral_typ   -> Size_t.to_int64 !@p, assign Size_t.of_int
    | Int32_t_integral_typ  -> Int32.to_int64  !@p, assign Int32.of_int
    | Int64_t_integral_typ  -> Int64.to_int64  !@p, assign Int64.of_int
    | Uint8_t_integral_typ  -> UInt8.to_int64  !@p, assign UInt8.of_int
    | Uint16_t_integral_typ -> UInt16.to_int64 !@p, assign UInt16.of_int
    | Uint32_t_integral_typ -> UInt32.to_int64 !@p, assign UInt32.of_int
    | Uint64_t_integral_typ -> UInt64.to_int64 !@p, assign UInt64.of_int

let find_field fields name =
  List.find (fun (Ctypes_static.BoxedField f) -> f.fname = name) fields

let field_integral_typ (type k): (k, _) Ctypes.field -> k integral_typ = fun f ->
  match f.ftype with
  | Primitive Schar    -> Schar_integral_typ
  | Primitive Short    -> Short_integral_typ
  | Primitive Int      -> Int_integral_typ
  | Primitive Int8_t   -> Int8_t_integral_typ
  | Primitive Int16_t  -> Int16_t_integral_typ
  | Primitive Char     -> Char_integral_typ
  | Primitive Uchar    -> Uchar_integral_typ
  | Primitive Long     -> Long_integral_typ
  | Primitive Llong    -> Llong_integral_typ
  | Primitive Ushort   -> Ushort_integral_typ
  | Primitive Sint     -> Sint_integral_typ
  | Primitive Uint     -> Uint_integral_typ
  | Primitive Ulong    -> Ulong_integral_typ
  | Primitive Ullong   -> Ullong_integral_typ
  | Primitive Size_t   -> Size_t_integral_typ
  | Primitive Int32_t  -> Int32_t_integral_typ
  | Primitive Int64_t  -> Int64_t_integral_typ
  | Primitive Uint8_t  -> Uint8_t_integral_typ
  | Primitive Uint16_t -> Uint16_t_integral_typ
  | Primitive Uint32_t -> Uint32_t_integral_typ
  | Primitive Uint64_t -> Uint64_t_integral_typ
  | t -> specification_error @@ Invalid_length_field { field_name = f.fname;
                                                       field_typ = BoxedType t }

let length_field_access (type struct_or_union)
    (aggreg_spec: ('k, struct_or_union) Ctypes.structured Ctypes.typ)
    length_field_name : field_access =
  match aggreg_spec with
  | Struct spec ->
      let BoxedField field =
        try find_field spec.fields length_field_name
        with Not_found ->
          specification_error @@ Unknown_field { field_name = length_field_name;
                                                 aggreg_type_tag = spec.tag }
      in
      {
        ap_suffix = Sc_C.Access_path.field length_field_name;
        length_field = Struct_field { struct_spec = spec; field;
                                      field_typ = field_integral_typ field };
      }
  | _ ->
      Fmt.invalid_arg "length_field_access"

let add_field defs (s: _ Ctypes.structured Ctypes.typ) (t, n, attrs) : _ =
  ctyp defs { f = fun (type k) (t: k Ctypes.typ) ->
      let field = Ctypes.field s n t in
      match t with
      | Abstract _ | Bigarray _ | Funptr _ | OCaml _ | Primitive _ | Qualified _
      | Struct _ | Union _ | View _ | Void | Array _ ->
          lazy ()
      | Pointer _ ->
          match Sc_C.Defs.pointer_validity attrs with
          | `Carray_with_unknown_length
          | `Carray_with_bound_length _
          | `Cstring as pv ->
              FieldH.add ptr_field_attrs (F field) pv;
              lazy ()
          | `Carray_with_length_field v ->
              lazy begin
                let field_access = length_field_access s v in
                FieldH.add ptr_field_attrs (F field)
                  (`Carray_with_length_field field_access)
              end
    } t

let widest_field = function
  | [] -> None
  | Ctypes_static.BoxedField f as bf :: tl ->
      let _, f =
        List.fold_left
          begin fun ((sz, _) as acc) ((Ctypes_static.BoxedField f) as bf) ->
            let fsz = Ctypes.sizeof (Ctypes.field_type f) in
            if fsz > sz then fsz, bf else acc
          end (Ctypes.sizeof (Ctypes.field_type f), bf) tl
      in
      Some f

(** [padding_bytes struct_spec] returns, for each field [f] of the given
    structure (considered in order), the number of padding bytes that are placed
    {e right after} [f] for alignment purposes. *)
let padding_bytes (s: _ Ctypes_static.structure_type) : int list =
  let open Ctypes_static in
  let rec aux acc cur_offset = function
    | [] ->
        append_global_padding cur_offset acc
    | BoxedField { ftype; foffset; _ } :: tl ->
        aux (foffset - cur_offset :: acc) (foffset + sizeof ftype) tl
  and append_global_padding cur_offset acc =
    match s.spec with
    | Complete { align = _; size } ->
        List.rev_append acc [size - cur_offset]
    | Incomplete _ ->
        List.rev acc
  in
  match aux [] 0 s.fields with
  | [] -> []
  | 0 :: tl -> tl    (* ignore padding before first field (should always be 0). *)
  | _ -> Fmt.failwith "Unexpected padding at begining of %a\
                      " format_typ' (Struct s)

let rec has_pointers: type v. v Ctypes.typ -> bool =
  let open Ctypes_static in
  function
  | Pointer _ ->
      true
  | Struct { fields; _ } ->
      List.exists (fun (BoxedField f) -> has_pointers (field_type f)) fields
  | Union { ufields; _ } ->
      List.exists (fun (BoxedField f) -> has_pointers (field_type f)) ufields
  | Array (typ, _) ->
      has_pointers typ
  | _ ->
      false

(* --- *)

(** {2 Direct access from CIL} *)

let sizeof_cil_typ ?(typdecls = empty_typdecls) typ =
  ctyp typdecls { f = fun t -> Ctypes.sizeof t } typ

(* --- *)

(** C pointers over existentially quantified types; used to build sets of
    pointers and memorize allocated array length. *)
module Ptr = struct
  type t = P: _ Ctypes.ptr -> t [@@unboxed]
  let box p =
    P p
  let raw (P p) =
    Ctypes.(raw_address_of_ptr @@ to_voidp p)
  let compare p q =
    Nativeint.compare (raw p) (raw q)
  let print ppf p =
    Fmt.pf ppf "0x%16nx" (raw p)
end

module Heap = struct
  include MakeMap (Ptr)
  type block_info =
    {
      len: int;
      kind: block_kind;
    }
  and block_kind = [`naked | `cstring]
  type ptr_view =                                  (* value in an actual heap *)
    | NULL
    | BLCK of int        (* allocated block of a given size, that may be zero *)
  type nonrec t = block_info t
  let add p len kind = add (Ptr.box p) { len; kind }
  let remove p = remove (Ptr.box p)
  let mem p = mem (Ptr.box p)
  let find p = find (Ptr.box p)
  let ptr_view (type v) : t -> v Ctypes.ptr -> ptr_view = fun h p ->
    if Ctypes.is_null p
    then NULL                                             (* NULL => len = -1 *)
    else BLCK (find p h).len
  let ptr_kind (type v) : t -> v Ctypes.ptr -> [block_kind | `null] =
    fun h p ->
    if Ctypes.is_null p
    then `null
    else ((find p h).kind :> [block_kind | `null])
end
type heap = Heap.t
type ptr_view = Heap.ptr_view

module type ENCODING_PARAMS = sig
  val encoding_params: encoding_params
end

module type ENCODING = sig module Params: ENCODING_PARAMS end

module type LOW_LEVEL_ENCODING = sig
  include ENCODING
  val release_ptr: 'k Ctypes.ptr -> heap -> heap
  val str_coercion: 'k Ctypes.typ -> string -> 'k
  val str_coercion': 'k Ctypes.typ -> 'k Ctypes.ptr -> string
  val read_value: 'k Ctypes.typ -> (int -> string) -> 'k * heap
  val write_value: 'k Ctypes.typ -> (string -> unit) -> h:heap -> 'k -> unit
  val memory_validity
    : elaborated_pointer_validity
    -> memory_validity

  (** [ptr_attrs ~pv depth] associates a pointer validity attribute with a depth
      (number of pointer indirections from the root inputs structure). *)
  val ptr_attrs
    : ?pv:elaborated_pointer_validity
    -> int
    -> ptr_attributes

  val max_pointed_size: _ Ctypes.typ -> memory_validity -> int

  val typ_size_bounds: any_ ctyp -> int * int

  val ctyp_fold_access_paths
    : _ Ctypes_static.typ
    -> ?max_depth:int
    -> ?f_prm:'a access_path_folder
    -> ?f_ptr:'a ptr_access_path_folder
    -> 'a -> 'a

  val ctyp_fold_direct_access_paths
    : _ Ctypes_static.typ
    -> ?f_prm:'a access_path_folder
    -> ?f_ptr:'a ptr_access_path_folder
    -> 'a -> 'a

  val ctyp_fold_reachable_pointers
    : _ Ctypes_static.typ
    -> ?max_depth:int
    -> f:'a ptr_attributes_folder
    -> merge:('a NEL.t -> 'a)
    -> 'a -> 'a

  val ctyp_visit_access_paths
    : _ Ctypes_static.typ
    -> ?max_depth:int
    -> ?f_prm:'a access_path_visitor
    -> ?f_ptr:'a ptr_access_path_visitor
    -> 'a -> 'a

  val cval_fold_pointers
    : deep:bool
    -> f:('a -> 'a) cptr_consumer
    -> h:heap
    -> 'k Ctypes_static.typ
    -> 'k
    -> 'a -> 'a
end

(** Encoding of dynamic data-structures to and from byte strings *)
module Encoding (Params: ENCODING_PARAMS) = struct
  module Params = Params

  open Ctypes
  open Ctypes_static

  let params = Params.encoding_params

  let ptr_decode (ptr_code, ub, ptr_depth) : ptr_view =
    if ptr_depth >= params.max_non_nil_ptr_depth || ptr_code == 0n
    then NULL
    else BLCK (if ptr_code < 0n
               then 0
               else Nativeint.(to_int (rem ptr_code @@ of_int (ub + 1))))

  let pp_c_ptr_decode ppf (ptr_code, lb, ub, ptr_depth) =
    Fmt.pf ppf "((%s >= %uu) || ((long)%s == 0l) ? -1l /* NULL */ : \
                (((long)%s < 0l) ? 0l /* non-NULL empty array */ :\
                ((long)%s %% ((long)%s - (long)%s + 1l))))"
      ptr_depth params.max_non_nil_ptr_depth
      ptr_code
      ptr_code
      ptr_code ub lb

  let constrained_carray_length (ptr_code, size, ub, ptr_depth) : ptr_view =
    if ptr_depth >= params.max_non_nil_ptr_depth || ptr_code == 0n
    then NULL
    else BLCK Unsigned.ULong.(to_int (rem (of_int64 size) @@ succ (of_int ub)))

  let pp_c_constrained_carray_length ppf (ptr_code, size, ub, ptr_depth) =
    Fmt.pf ppf "((%s >= %uu) || ((unsigned long)(%s) == 0ul) ? -1l /* NULL */ : \
                (long)((unsigned long)(%s) %% ((unsigned long)(%s) + 1ul)))"
      ptr_depth params.max_non_nil_ptr_depth
      ptr_code
      size ub

  let ptr_encode: ptr_view -> nativeint = function
    | NULL ->
        0n
    | BLCK 0 ->                                                 (* empty array *)
        -1n
    | BLCK len ->
        Nativeint.of_int len

  let cstring_buff_length_decode (ptr_code, ptr_depth) : ptr_view =
    if ptr_depth >= params.max_non_nil_ptr_depth ||
       Nativeint.(equal (logand 0x1n ptr_code) zero)
    then NULL
    else BLCK params.max_cstring_length

  let cstring_length_encode: ptr_view -> nativeint = function
    | _ -> 1n                                           (* For now: never NULL *)

  let pp_c_cstring_buff_length_decode ppf (ptr_code, ptr_depth) =
    Fmt.pf ppf "(((%s >= %uu) || ((%s & 1ul) == 0ul)) ? 0ul : %ul)"
      ptr_depth params.max_non_nil_ptr_depth
      ptr_code (params.max_cstring_length + 1)

  let str_coercion t s =
    !@(coerce string (ptr void) s |>
       coerce (ptr void) (ptr t))

  let str_coercion' t p =
    (coerce (ptr t) (ptr char) p |>
     string_from_ptr ~length:(sizeof t))

  let memory_validity: elaborated_pointer_validity -> memory_validity = function
    | #memory_validity as pv -> pv
    | `Carray_with_unknown_length ->
        `Carray_with_bound_length params.max_ptr_array_length

  (** [max_pointed_size typ pv] is the maximum size {b in bytes} of an array of
      elements of type [typ] that may be pointed to by a pointer with validity
      [pv]. *)
  let max_pointed_size t : memory_validity -> int = function
    | `Carray_with_bound_length i ->
        sizeof @@ array i t      (* size of an array of [i] cells of type [t] *)
    | `Carray_with_length_field _ ->
        (* Note: assumes length <= max_ptr_array_length *)
        sizeof @@ array params.max_ptr_array_length t
    | `Cstring ->                    (* + 1 to account for terminating '\000'  *)
        params.max_cstring_length + 1

  (** [reset_padding_bytes structure_spec structure] resets every padding byte
      of [structure] to [0] *)
  let reset_padding_bytes (type v) : v structure_type -> v structure -> unit =
    fun s v ->
    List.iter2 begin fun (BoxedField ({ ftype; _ } as f)) padding ->
      if padding <> 0 then
        let fp = coerce (ptr ftype) (ptr char) (v @. f) +@ sizeof ftype in
        for i = 0 to padding - 1; do
          fp +@ i <-@ '\000'
        done
    end s.fields (padding_bytes s)

  let error_on_union_with_pointers ({ utag; _ } as u) =
    if has_pointers (Union u) then                  (* TODO: custom exception *)
      Fmt.failwith "Unsupported union type (`%s`) with pointers" utag

  (** [read_value t read] decodes a possibly non-flat C data-structure via
      successive reads of character strings using [read].  The latter is a
      function that is given an integer [n], and either returns a string [n]
      bytes or raises {!Invalid_argument}.  [read] typically reads sequentially
      from a file or string. *)
  let read_value t read =

    (* NB: assumes we never attach attributes to items in arrays of pointers *)

    let rec decode
      : type v. (heap -> int -> ?blk:heap_block -> v typ -> v * heap) =
      fun h depth ?blk t ->
        let s =
          try read (sizeof t)
          with Invalid_argument msg ->
            Fmt.invalid_arg "decode/%s/: %s" (string_of_typ t) msg
        in
        match t with
        | Void ->
            (), h
        | View _ | Primitive _ ->
            str_coercion t s, h
        | Struct spec ->
            let v = str_coercion t s in
            let blk = Heap_block (spec, addr v) in
            reset_padding_bytes spec v;
            v, update_fields (Heap.add (addr v) 1 `naked h) depth ~blk t v
        | Union _ ->
            let v = str_coercion t s in
            v, update_fields (Heap.add (addr v) 1 `naked h) depth ?blk t v
        | Pointer typ ->
            decode_ptr h depth ?blk typ (str_coercion t s)
        | Array (typ, len) ->
            let p = CArray.from_ptr (coerce string (ptr typ) s) len in
            let h = Heap.add (CArray.start p) len `naked h in
            p, update_fields h depth ?blk t p
        | t ->
            Fmt.failwith "Unsupported translation from string to value of \
                          type %a" format_typ' t

    and update_fields
      : type v. heap -> int -> ?blk:_ -> v typ -> v -> heap =
      fun h depth ?blk t v ->
        match t with
        | Struct ({ fields; _ } as spec) ->
            let blk = Heap_block (spec, addr v) in               (* new frame *)
            List.fold_left begin fun h (BoxedField f) -> match field_type f with
              | Pointer t' ->
                  let pv = ptr_field_validity f in
                  let p, h = decode_ptr h depth ~pv ~blk t' (getf v f) in
                  setf v f p; h
              | t ->
                  update_fields h depth t (getf v f)
            end h fields
        | Union u ->                 (* FIXME: handling of pointers in unions? *)
            error_on_union_with_pointers u;
            h
        | Array (Pointer t, len) ->
            Basics.SeqUtils.seq len |> Seq.fold_left begin fun h i ->
              let p, h = decode_ptr h depth ?blk t (CArray.get v i) in
              CArray.set v i p; h
            end  h
        | Array (t, len) ->
            Basics.SeqUtils.seq len |> Seq.fold_left begin fun h i ->
              update_fields h depth ?blk t (CArray.get v i)
            end h
        | _ ->
            h

    and decode_ptr
      : type v. (heap -> int -> ?pv:elaborated_pointer_validity -> ?blk:_ ->
                 v Ctypes.typ -> v ptr -> v ptr * heap) =
      fun h depth ?(pv = single_cell_validity) ?blk typ p ->
        let len, kind =
          let ptr_code =
            if is_null p then 0n else raw_address_of_ptr (to_voidp p)
          in
          match memory_validity pv with
          | `Cstring ->
              cstring_buff_length_decode (ptr_code, depth), `cstring
          | `Carray_with_bound_length l ->
              ptr_decode (ptr_code, l, depth), `naked
          | `Carray_with_length_field { length_field; _ } ->
              (* Note: until we attach proper constraints on associated length
                 fields, we need to fix their values so they do not exceed
                 [max_ptr_array_length].  This is actually "easier" to do while
                 decoding constrained pointers. *)
              let len0, set_length_field =
                struct_field_as_length ~blk:(Option.get blk) length_field in
              let ub = params.max_ptr_array_length in
              let len = constrained_carray_length (ptr_code, len0, ub, depth) in
              set_length_field (match len with NULL -> 0 | BLCK len -> len);
              len, `naked
        in
        match len with
        | NULL ->
            from_voidp typ null, h
        | BLCK len ->
            let array, h = decode h (succ depth) ?blk (Array (typ, len)) in
            let p = CArray.start array in
            p, Heap.add p len kind h
    in

    (* NB: assumes we never decode directly from a pointer *)
    decode Heap.empty 0 t


  (** [write_value t write ~h v] encodes a possibly non-flat C data-structure
      [v] via successive writes of character strings using [write].  The latter
      is a procedure that is given successive strings, whose concatenation forms
      the encoded result: [v] can be recovered by feeding [read_value] with the
      resulting string.  *)
  let write_value t write ~h =

    let rec encode
      : type v. (elaborated_pointer_validity option -> v Ctypes.typ -> v -> unit) =
      fun pv t v ->
        match t with
        | Void ->
            ()
        | View _ | Primitive _ ->
            write (str_coercion' t (allocate t v))
        | Struct _ | Union _ ->
            let p = allocate t v in
            update_fields t !@p;
            write (str_coercion' t p);
            encode_referenced t v
        | Pointer typ ->
            write (encode_ptr ?pv typ v |> allocate t |> str_coercion' t);
            encode_referenced t v
        | Array (typ, _) ->
            let v': v = CArray.copy v in
            update_fields t v';
            write (coerce (ptr typ) (ptr char) (CArray.start v') |>
                   string_from_ptr ~length:(sizeof t));
            encode_referenced t v
        | t ->
            Fmt.failwith "Unsupported translation from string to value of \
                          type %a" format_typ' t

    (* Update pointer fields of v' in place, for the encoding of a single flat
       value *)
    and update_fields: type v. v typ -> v -> unit = fun t v' -> match t with
      | Struct { fields; _ } ->
          List.iter begin fun (BoxedField f) -> match field_type f with
            | Pointer typ ->
                let pv = ptr_field_validity f in
                setf v' f (encode_ptr ~pv typ (getf v' f))
            | t ->
                update_fields t (getf v' f)
          end fields
      | Union u ->
          error_on_union_with_pointers u
      | Array (Pointer t, len) ->
          Basics.SeqUtils.seq len |> Seq.iter begin fun i ->
            CArray.set v' i (encode_ptr t (CArray.get v' i))
          end
      | Array (t, len) ->
          Basics.SeqUtils.seq len |> Seq.iter begin fun i ->
            update_fields t (CArray.get v' i)
          end
      | _ ->
          ()

    (* Traverse every pointer of [v] to perform the depth-first encoding *)
    and encode_referenced: type v. v Ctypes.typ -> v -> unit = fun t v ->
      match t with
      | Void | View _ | Primitive _ ->
          ()
      | Struct { fields; _ } ->
          List.iter begin fun (BoxedField f) ->
            encode_referenced (field_type f) (getf v f)
          end fields
      | Union u ->
          error_on_union_with_pointers u
      | Pointer t ->
          encode_referenced_by_pointer t v
      | Array (t, len) ->
          Basics.SeqUtils.seq len |> Seq.iter begin fun i ->
            encode_referenced t (CArray.get v i)
          end
      | _ ->
          ()

    and encode_referenced_by_pointer: type v. v Ctypes.typ -> v ptr -> unit =
      fun t p ->
        match Heap.ptr_view h p with
        | NULL ->
            ()
        | BLCK len ->
            encode None (Array (t, len)) (CArray.from_ptr p len)

    and encode_ptr
      : type v. (?pv:elaborated_pointer_validity -> v Ctypes.typ -> v ptr ->
                 v ptr) =
      fun ?(pv = single_cell_validity) typ p ->
        match Heap.ptr_view h p with
        | NULL ->
            from_voidp typ null
        | BLCK _ as len ->
            from_voidp typ @@ ptr_of_raw_address @@
            match memory_validity pv with
            | `Cstring ->
                cstring_length_encode len
            | `Carray_with_bound_length _ ->
                ptr_encode len
            | `Carray_with_length_field _ ->
                Nativeint.one                     (* only encode non-NULLness *)

    in

    (* NB: as in [read_value], assumes we never encode directly from a
       pointer *)
    encode None t

  let ptr_attrs ?(pv = single_cell_validity) depth =
    {
      depth;
      pv = memory_validity pv;
    }

  let visit ~continue = function
    | Do_children acc -> continue acc
    | Do_children_and_then (acc, f) -> f @@ continue acc
    | Skip_children acc -> acc

  let ctyp_visit_access_paths typ
      ?(max_depth = params.max_non_nil_ptr_depth)
      ?(f_prm = { f = fun _ _ acc -> Do_children acc })
      ?(f_ptr = { f = fun _ _ _ acc -> Do_children acc })
    =
    let module AP = Sc_C.Access_path in
    let rec aux: type v. v Ctypes.typ -> _ = fun t ap p_attrs acc ->
      match t with
      | Struct s ->
          visit ~continue:(fields s.fields ap p_attrs) @@ f_prm.f t ap acc
      | Union u ->
          visit ~continue:(fields u.ufields ap p_attrs) @@ f_prm.f t ap acc
      | Pointer t ->
          visit ~continue:(pointer t ap p_attrs) @@ f_ptr.f t ap p_attrs.pv acc
      | Array (typ, len) ->
          visit ~continue:(array typ len ap p_attrs) @@ f_prm.f t ap acc
      | Abstract _ | Bigarray _ | Funptr _ | OCaml _ | Primitive _ | Qualified _
      | View _ | Void ->
          visit ~continue:Fun.id @@ f_prm.f t ap acc
    and fields: type k. k boxed_field list -> _ = fun fields ap p_attrs acc ->
      List.fold_left begin fun acc (BoxedField f) ->
        aux (field_type f)
          (Some (AP.append_to_suffix' ap @@ AP.field @@ field_name f))
          (ptr_attrs ~pv:(ptr_field_validity f) p_attrs.depth) acc
      end acc fields
    and pointer: type v. v Ctypes.typ -> _ = fun t ap p_attrs acc ->
      if p_attrs.depth >= max_depth then acc else
        let len = match p_attrs.pv with
          | `Carray_with_bound_length i -> i
          | `Carray_with_length_field _ -> params.max_ptr_array_length
          | `Cstring -> params.max_cstring_length
        in
        array t len ap (ptr_attrs @@ succ p_attrs.depth) acc
    and array: type v. v Ctypes.typ -> int -> _ = fun typ len ap p_attrs acc ->
      Basics.SeqUtils.seq len |> Seq.fold_left begin fun acc i ->
        aux typ (Some (AP.append_to_suffix' ap @@ AP.index i))
          (ptr_attrs p_attrs.depth) acc
      end acc
    in
    aux typ None (ptr_attrs 0)

  let ctyp_fold_access_paths typ
      ?(max_depth = params.max_non_nil_ptr_depth)
      ?(f_prm = { f = fun _ _ acc -> acc })
      ?(f_ptr = { f = fun _ _ _ acc -> acc })
    =
    let module AP = Sc_C.Access_path in
    let rec aux: type v. v Ctypes.typ -> _ = fun t ap p_attrs acc ->
      match t with
      | Struct s ->
          fields s.fields ap p_attrs @@ f_prm.f t ap acc
      | Union u ->
          fields u.ufields ap p_attrs @@ f_prm.f t ap acc
      | Pointer t ->
          pointer t ap p_attrs acc
      | Array (typ, len) ->
          array typ len ap p_attrs @@ f_prm.f t ap acc
      | Abstract _ | Bigarray _ | Funptr _ | OCaml _ | Primitive _ | Qualified _
      | View _ | Void ->
          f_prm.f t ap acc
    and fields: type k. k boxed_field list -> _ = fun fields ap p_attrs acc ->
      List.fold_left begin fun acc (BoxedField f) ->
        aux (field_type f)
          (Some (AP.append_to_suffix' ap @@ AP.field @@ field_name f))
          (ptr_attrs ~pv:(ptr_field_validity f) p_attrs.depth) acc
      end acc fields
    and pointer: type v. v Ctypes.typ -> _ = fun t ap p_attrs acc ->
      let acc = f_ptr.f t ap p_attrs.pv acc in
      if p_attrs.depth >= max_depth then acc else
        let len = match p_attrs.pv with
          | `Carray_with_bound_length i -> i
          | `Carray_with_length_field _ -> params.max_ptr_array_length
          | `Cstring -> params.max_cstring_length
        in
        array t len ap (ptr_attrs @@ succ p_attrs.depth) acc
    and array: type v. v Ctypes.typ -> int -> _ = fun typ len ap p_attrs acc ->
      Basics.SeqUtils.seq len |> Seq.fold_left begin fun acc i ->
        aux typ (Some (AP.append_to_suffix' ap @@ AP.index i))
          (ptr_attrs p_attrs.depth) acc
      end acc
    in
    aux typ None (ptr_attrs 0)

  let ctyp_fold_direct_access_paths typ =
    ctyp_fold_access_paths typ ~max_depth:0

  let ctyp_fold_reachable_pointers typ
      ?(max_depth = params.max_non_nil_ptr_depth)
      ~f:{ f } ~merge
    =
    let rec aux: type v. v Ctypes.typ -> _ = fun t ({ pv = _; depth } as p_attrs) acc ->
      let struct_ { fields; _ } =
        List.fold_left begin fun acc (BoxedField f) ->
          aux (field_type f) (ptr_attrs ~pv:(ptr_field_validity f) depth) acc
        end acc fields
      and union_ { ufields; _ } =
        NEL.rev_map begin fun (BoxedField f) ->
          aux (field_type f) (ptr_attrs ~pv:(ptr_field_validity f) depth) acc
        end (NEL.of_rev_list ufields) |> merge
      in
      match t with
      | Pointer t ->
          let acc = f t p_attrs acc in
          if depth >= max_depth
          then acc
          else aux t (ptr_attrs (succ depth)) acc
      | Struct s ->
          struct_ s
      | Union u ->
          union_ u
      | Array (typ, _len) ->             (* Fixed-length array, but visit once *)
          aux typ (ptr_attrs depth) acc
      | Abstract _ | Bigarray _ | Funptr _ | OCaml _ | Primitive _ | Qualified _
      | View _ | Void ->
          acc
    in
    aux typ (ptr_attrs 0)

  let typ_size_bounds t =
    let rec max_ctyp_size: type v. v Ctypes.typ -> int -> int = fun t depth ->
      let size = sizeof t in
      if depth >= params.max_non_nil_ptr_depth then size else
        ctyp_fold_reachable_pointers t ~max_depth:0 size
          ~merge:(NEL.fold_left ~f:max 0)
          ~f:{
            f = fun (type k) (t: k Ctypes.typ) { pv; _ } ->
              match pv with
              | `Cstring ->
                  (+) params.max_cstring_length
              | `Carray_with_bound_length ub ->
                  (+) (max_ctyp_size t (succ depth) * ub)
              | `Carray_with_length_field _ ->
                  (* Note: assumes length <= max_ptr_array_length *)
                  (+) (max_ctyp_size t (succ depth) * params.max_ptr_array_length)
          }
    in
    typ_size t, as_ctyp ~f:{ f = fun t -> max_ctyp_size t 0 } t

  let cval_fold_pointers ~deep ~(f: _ cptr_consumer) ~h =
    let rec aux: type v. v Ctypes.typ -> v -> _ = fun t v p_attrs acc ->
      match t with
      | Pointer t ->
          aux_ptr t v p_attrs acc
      | Struct { fields; _ } ->
          List.fold_left begin fun acc (BoxedField f) ->
            let p_attrs = ptr_attrs ~pv:(ptr_field_validity f) p_attrs.depth in
            aux (field_type f) (getf v f) p_attrs acc
          end acc fields
      | Union u ->
          error_on_union_with_pointers u;
          acc
      | Array (typ, _) ->
          let p_attrs = ptr_attrs p_attrs.depth in
          CArray.fold_left (fun acc v -> aux typ v p_attrs acc) acc v
      | Abstract _ | Bigarray _ | Funptr _ | OCaml _ | Primitive _ | Qualified _
      | View _ | Void ->
          acc
    and aux_ptr: type v. v Ctypes.typ -> v Ctypes.ptr -> _ = fun t p p_attrs acc ->
      match p_attrs.pv with
      | `Cstring ->
          let p_attrs = ptr_attrs ~pv:`Cstring p_attrs.depth in
          f.p t p ~len:params.max_cstring_length p_attrs acc
      | `Carray_with_bound_length _
      | `Carray_with_length_field _ ->
          let len = Heap.ptr_view h p in
          let acc =
            f.p t p (ptr_attrs p_attrs.depth) acc
              ?len:(match len with NULL -> None | BLCK len -> Some len)
          in
          if not deep then acc else begin
            let p_attrs = ptr_attrs @@ succ p_attrs.depth in
            match len with
            | NULL | BLCK 0 ->
                acc
            | BLCK len ->
                aux (array len t) (CArray.from_ptr p len) p_attrs acc
          end
    in
    fun t v -> aux t v (ptr_attrs 0)

  let release_ptr: type a. a ptr -> heap -> heap = fun p h ->
    if is_null p then h else
      cval_fold_pointers ~deep:true ~h ~f:{
        p = fun _ p ?len:_ _ h ->
          if not (is_null p) && Heap.mem p h then Heap.remove p h else h
      } (reference_type p) !@p h

end

(* --- *)

module type VALUE_PRINTER = sig
  type literal_memory = literal_assignments * literal_heap
  and literal_assignments
  and literal_heap
  and instructions
  type c_code =
    {
      pp_heap: static:bool -> PPrt.pu;
      pp_globals: PPrt.pu;
      pp_locals: PPrt.pu;
    }
  val literal_memory_as_c_code
    : globals: Sc_C.Types.vars
    -> locals: Sc_C.Types.vars
    -> literal_memory
    -> c_code
  val instructions_as_c_code
    : globals: Sc_C.Types.vars
    -> locals: Sc_C.Types.vars
    -> instructions
    -> c_code
end

module Printer = struct

  open Ctypes

  let pp_c_typ_: type k. ?var:string -> k typ PPrt.pp = fun ?var ->
    let rec aux: type k. int -> k typ PPrt.pp =
      fun ptr_depth ppf -> function
        | Array (typ, len) ->
            Fmt.pf ppf "%a[%d]" (aux ptr_depth) typ len
        | Pointer t ->
            aux (ptr_depth + 1) ppf t
        | t ->
            Fmt.pf ppf "%a%a" pp_typ t
              (fun ppf -> function
                 | 0 -> pp_var ppf
                 | n -> Fmt.pf ppf " (%s%t)" (String.make n '*') pp_var)
              ptr_depth
    and pp_typ: type k. k typ PPrt.pp = fun ppf -> function
      | Struct { tag; _ } -> Fmt.pf ppf "struct %s" tag
      | Union { utag; _ } -> Fmt.pf ppf "union %s" utag
      | t -> format_typ' ppf t
    and pp_var ppf = match var with
      | None -> ()
      | Some v -> Fmt.pf ppf " %s" v
    in
    fun ppf t -> aux 0 ppf t

  let c_typ: type k. k typ PPrt.pp =
    fun ppf t -> pp_c_typ_ ?var:None ppf t

  let c_decl: type k. (k typ * string) PPrt.pp =
    fun ppf (t, var) -> pp_c_typ_ ~var ppf t

  let cil_typ: Cil.typ PPrt.pp =
    fun ppf t -> ctyp empty_typdecls { f = fun t -> pp_c_typ_ ?var:None ppf t } t

  let cil_decl: (Cil.typ* string) PPrt.pp =
    fun ppf (t, var) -> ctyp empty_typdecls { f = fun t -> pp_c_typ_ ~var ppf t } t

  let assignable_typ: type k. k typ -> bool = function
    | Primitive _ | View _ | Pointer _ -> true
    | _ -> false

  (* --- *)

  module Values (Encoding: ENCODING) = struct
    open Ctypes_static

    let params = Encoding.Params.encoding_params

    let pp_instrs ?(with_terminating_semicolon = true) pp_instr ppf instrs =
      let fclose: _ format6 =
        if with_terminating_semicolon then ";@]" else "@]"
      in
      PPrt.pp_lst ~fopen:"@[<v>" ~fsep:";@;" ~fclose ~fempty:""
        pp_instr ppf instrs

    (** The type of literal heaps, which describes a set of non-overlapping
        memory cells that can each be individually addressed.  Note the values
        of these cells is not dynamically allocated via `malloc` (the data all
        lives in `.bss` or `.data` sections). *)
    type literal_heap = literal_heap_object list

    and literal_heap_object =
      {
        obj_typ: boxed_typ;
        obj_var: string;
        obj_lit: string;
      }

    let empty_heap: literal_heap = []

    let print_literal_heap ?(static = true) : literal_heap PPrt.pp = fun ppf h ->
      if static
      then begin         (* ok for now as we only have tree-like structures *)
        pp_instrs begin fun ppf { obj_typ = BoxedType t; obj_var; obj_lit } ->
          Fmt.pf ppf "%a = %s" c_decl (t, obj_var) obj_lit
        end ppf h
      end else if h <> [] then begin
        Fmt.pf ppf "@[<v>";
        pp_instrs begin fun ppf { obj_typ = BoxedType t; obj_var; _ } ->
          c_decl ppf (t, obj_var)
        end ppf h;
        Fmt.pf ppf "@;";
        pp_instrs begin fun ppf { obj_typ = BoxedType t; obj_var; obj_lit } ->
          if assignable_typ t
          then Fmt.pf ppf "%s = %s" obj_var obj_lit
          else Fmt.pf ppf "(void) memcpy (@[&(%s),@ (%a)%s,@ sizeof (%a))@]\
                          " obj_var c_typ t obj_lit c_typ t
        end ppf h;
        Fmt.pf ppf "@]";
      end

    type lit =
      | Lit of string
      | Arr of string
      | Ref of string
      | Str of string * int

    module AP = Sc_C.Access_path

    type literal_memory = literal_assignments * literal_heap
    and literal_assignments = literal_assignment list

    (** Type of C variable assignment with literals *)
    and literal_assignment =
      {
        typ: Ctypes_static.boxed_typ;
        var: string;
        lit: lit;
      }

    and instructions = instruction list
    and instruction =
      | Assignment of literal_assignment
      | Alloc of { typ: boxed_typ; length: int; ptr_ap: AP.t; init: instructions }

    let pp_field_name ppf f =
      Fmt.pf ppf ".%s = " (field_name f)

    let pp_bracketed pp =
      PPrt.pp_lst ~fopen:"{@[<hov>" ~fsep:",@;" ~fclose:"@]}" ~fempty:"{}" pp

    let pp_cstring ~with_padding ~escape ppf v =
      let str = Ctypes.string_from_ptr ~length:params.max_cstring_length v in
      let escape = if escape then String.escaped else fun s -> s in
      Fmt.pf ppf "\"%s\"" @@ escape @@
      (match if with_padding then None else String.index_opt str '\000' with
       | None -> str
       | Some z -> String.sub str 0 z)

    let str_of_lit = function
      | Lit l | Arr l | Str (l, _) -> l
      | Ref r -> "&"^r

    let as_c_literal
      : type v. (v Ctypes.typ -> ?lit_heap:literal_heap -> h:heap -> v ->
                 lit * literal_heap) =
      fun t ?(lit_heap = empty_heap) ~h v ->
      let c_lit fmt = PPrt.kasprintf (fun s -> Lit s) fmt
      and c_arr fmt = PPrt.kasprintf (fun s -> Arr s) fmt
      and c_ref fmt = PPrt.kasprintf (fun s -> Ref s) fmt
      and c_str fmt l = PPrt.kasprintf (fun s -> Str (s, l)) fmt in
      let lit_heap_ref = ref []
      and lit_heap_num = ref (List.length lit_heap) in
      let rec str: type v. v Ctypes.typ -> v -> string = fun t v ->
        str_of_lit @@ aux t v
      and aux: type v. v Ctypes.typ -> v -> lit =
        let field_assign v (BoxedField f) =
          PPrt.asprintf "%a%s" pp_field_name f @@ str (field_type f) (getf v f)
        in
        fun t v -> match t with
          | Struct { fields; _ } ->
              c_lit "%a" (pp_bracketed Fmt.string) @@
              List.map (field_assign v) fields
          | Union { ufields; _ } ->
              let fa = match widest_field ufields with
                | None -> []
                | Some f -> [field_assign v f]
              in
              c_lit "%a" (pp_bracketed Fmt.string) fa
          | Array (typ, _) ->
              let v = CArray.to_list v in
              c_arr "%a" (pp_bracketed Fmt.string) (List.map (str typ) v)
          | Pointer Primitive Char when Heap.ptr_kind h v = `cstring ->
              (* TODO: Parameterize padding of string literals *)
              c_str "%a" (params.max_cstring_length + 1)
                (pp_cstring ~with_padding:true ~escape:true) v
          | Pointer typ ->
              aux_ptr typ v
          | Primitive Char ->
              if Char.code v < 128
              then c_lit "'%s'" (Char.escaped v)
              else c_lit "%u" (Char.code v)
          | Primitive Float ->
              if Float.is_nan v   (* CHECKME: quiet/signaling, infinity, etc? *)
              then c_lit "(float)(0.0/0.0) /*NaN*/"
              else c_lit "%h /*%g*/" v v
          | Primitive Double ->
              if Float.is_nan v                   (* CHEKME: ditto float case *)
              then c_lit "(double)(0.0F/0.0F) /*NaN*/"
              else c_lit "%H /*%G*/" v v
          | t ->
              c_lit "%a" (format t) v
      and aux_ptr: type v. v Ctypes.typ -> v ptr -> lit = fun typ v ->
        (* XXX: Could here check [v] is not in heap already.  Ok for now, given
           we only deal with cycle-less data-structures. *)
        match Heap.ptr_view h v with
        | NULL ->
            c_lit "NULL"
        | BLCK len ->
            let obj_typ, obj_lit, deref =
              if len = 1 then
                BoxedType typ, str typ !@v, true
              else
                let typ = Array (typ, len) in
                BoxedType typ, str typ (CArray.from_ptr v len), false
            in
            let obj_var = Fmt.str "_heap_obj_%u_" !lit_heap_num in
            incr lit_heap_num;
            lit_heap_ref := { obj_typ; obj_var; obj_lit } :: !lit_heap_ref;
            (if deref then c_ref else c_lit) "%s" obj_var
      in
      let s = aux t v in
      s, List.rev_append !lit_heap_ref lit_heap

    let fields_as_c_literal_assignments { fields; _ } v h : literal_memory =
      let acc, lit_heap =
        List.fold_left begin fun (acc, lit_heap) (BoxedField f) ->
          let var = field_name f and ft = field_type f in
          let lit, lit_heap = as_c_literal ft ~lit_heap ~h (getf v f) in
          { typ = BoxedType ft; var; lit } :: acc, lit_heap
        end ([], empty_heap) fields in
      List.rev acc, lit_heap

    let initialize typ ap v ~h : instructions =
      let literal typ ap v =
        [ Assignment { typ = BoxedType typ;
                       var = AP.to_string ap;
                       lit = fst @@ as_c_literal typ ~h v } ]
      and strcpy ap v len =
        let str = Fmt.str "%a" (pp_cstring ~with_padding:true ~escape:true) v in
        [ Assignment { typ = BoxedType (array len char);
                       var = AP.to_string ap;
                       lit = Str (str, len) } ]
      and alloc etyp length ptr_ap init =
        [ Alloc { typ = BoxedType etyp; length; ptr_ap; init } ]
      in
      let rec aux: type v. v Ctypes.typ -> AP.t -> v -> instructions =
        fun typ ap v ->
          let field_access v (BoxedField f) =
            aux (field_type f) (AP.append_field ap @@ field_name f) (getf v f)
          in
          match typ with
          | Primitive _ | View _ ->
              literal typ ap v
          | typ when not (has_pointers typ) ->
              literal typ ap v    (* note: every case below is about pointers *)
          | Pointer Primitive Char when Heap.ptr_kind h v = `cstring ->
              let buff_len = params.max_cstring_length + 1 in
              alloc char buff_len ap @@ strcpy ap v buff_len
          | Pointer typ ->
              pointer typ ap v
          | Array (etyp, len) ->
              List.flatten @@ List.init len @@
              fun i -> aux etyp (AP.append_index ap i) (CArray.get v i)
          | Struct { fields; _ } ->
              List.concat_map (field_access v) fields
          | Union { ufields; _ } ->
              (* union with pointers should be avoided, but in case. *)
              Option.fold ~none:[] ~some:(field_access v) (widest_field ufields)
          | Abstract _ | Bigarray _ | Funptr _ | OCaml _ | Qualified _ | Void ->
              Logs.warn (fun p -> p "%s:%s value of type %a is unexpected here"
                            __MODULE__ __LOC__ c_typ typ);
              []
      and pointer: type v. v Ctypes.typ -> AP.t -> v ptr -> instructions =
        fun typ ap v ->
          match Heap.ptr_view h v with
          | NULL ->
              literal (ptr typ) ap v
          | BLCK 0 ->
              alloc typ 0 ap []
          | BLCK len ->
              alloc typ len ap @@ aux (array len typ) ap (CArray.from_ptr v len)
      in
      aux typ ap v

    let fields_as_c_allocations { fields; _ } v h : instructions =
      let acc =
        List.fold_left begin fun acc (BoxedField f) ->
          let var = field_name f and ft = field_type f in
          initialize ft (AP.origin_only var) (getf v f) ~h @ acc
        end [] fields in
      List.rev acc

    let as_c_literal t ?lit_heap ~h v =
      let lit, lit_heap = as_c_literal t ?lit_heap ~h v in
      str_of_lit lit, lit_heap

    let pp_pseudo_literal t ~h ppf =
      let rec pp_aux: type v. v Ctypes.typ -> _ -> v -> unit = fun t ppf v ->
        let pp_field_assign v ppf (BoxedField f) =
          let v = getf v f in
          Fmt.pf ppf "@[<hov 2>%a@,%a@]" pp_field_name f (pp_aux (field_type f)) v
        in
        match t with
        | Struct { fields; _ } ->
            pp_bracketed (pp_field_assign v) ppf fields
        | Union { ufields; _ } ->
            pp_bracketed (pp_field_assign v) ppf        (* print widest field *)
              (match widest_field ufields with None -> [] | Some f ->  [f])
        | Array (etyp, _) ->
            pp_bracketed (pp_aux etyp) ppf (CArray.to_list v)
        | Pointer Primitive Char when Heap.ptr_kind h v = `cstring ->
            pp_cstring ~with_padding:false ~escape:true ppf v
        | Pointer t ->
            pp_ptr t ppf v
        | Primitive Char ->
            if Char.code v < 128
            then Fmt.pf ppf "'%s'" (Char.escaped v)
            else Fmt.pf ppf "%u" (Char.code v)
        | t ->
            format t ppf v
      and pp_ptr: type v. v Ctypes.typ -> _ -> v ptr -> unit = fun t ppf v ->
        match Heap.ptr_view h v with
        | NULL ->
            Fmt.string ppf "NULL"                  (* TODO: transmit stddef.h *)
        | BLCK 1 ->
            Fmt.pf ppf "&%a" (pp_aux t) !@v
        | BLCK len ->
            Fmt.pf ppf "&%a" (pp_aux (Array (t, len))) (CArray.from_ptr v len)
      in
      pp_aux t ppf

    (* --- *)

    let memcpy: type v. _ -> (v typ * _ * _) -> _ = fun ppf (typ, dst, lit) ->
      Fmt.pf ppf "(void) memcpy (@[&(%s),@ &(%a)%s,@ sizeof (%a))@]\
                 " dst c_typ typ lit c_typ typ     (* TODO: transmit string.h *)

    let memcpy_array: type v. _ -> (v typ * _ * _) -> _ = fun ppf (typ, dst, lit) ->
      Fmt.pf ppf "(void) memcpy (@[%s,@ (%a)%s,@ sizeof (%a))@]\
                 " dst c_typ typ lit c_typ typ

    let memcpy_cstring ppf (dst_buff, buff_len, strlit) =
      Fmt.pf ppf "(void) memcpy (@[%s,@ %s,@ sizeof (%a))@]\
                 " dst_buff strlit c_typ (array buff_len char)

    let pp_assignment ~declare ppf { typ = BoxedType typ; var; lit } =
      if declare
      then match lit with
        | Ref o ->
            Fmt.pf ppf "@[<2>%a =@ &%s@]" c_decl (typ, var) o
        | Lit l when assignable_typ typ ->
            Fmt.pf ppf "@[<2>%a =@ %s@]" c_decl (typ, var) l
        | Lit l ->
            Fmt.pf ppf "@[<2>%a@];@;" c_decl (typ, var);
            memcpy ppf (typ, var, l)
        | Arr l ->
            Fmt.pf ppf "@[<2>%a@];@;" c_decl (typ, var);
            memcpy_array ppf (typ, var, l)
        | Str (l, buff_len) ->
            Fmt.pf ppf "@[<2>%a@];@;" c_decl (array buff_len char, var);
            memcpy_cstring ppf (var, buff_len, l)
      else match lit with
        | Ref o ->
            Fmt.pf ppf "@[<2>%s =@ &%s@]" var o
        | Lit l when assignable_typ typ ->
            Fmt.pf ppf "@[<2>%s =@ %s@]" var l
        | Lit l ->
            memcpy ppf (typ, var, l)
        | Arr l ->
            memcpy_array ppf (typ, var, l)
        | Str (l, buff_len) ->
            memcpy_cstring ppf (var, buff_len, l)

    let rec pp_instruction ~declare ppf = function
      | Assignment a ->
          pp_assignment ~declare ppf a
      | Alloc { typ = BoxedType t; length; ptr_ap; init } ->
          Fmt.pf ppf "@[<2>%a =@ malloc (sizeof (%a))@]%a"
            (if declare then c_decl else Fmt.using snd Fmt.string)
            (ptr t, AP.to_string ptr_ap) c_typ (array length t)
            (if init = [] then Fmt.nop else Fmt.any ";@;") ();
          pp_instrs (pp_instruction ~declare:false) ppf init
            ~with_terminating_semicolon:false

    type c_code =
      {
        pp_heap: static:bool -> PPrt.pu;
        pp_globals: PPrt.pu;
        pp_locals: PPrt.pu;
      }

    let literal_memory_as_c_code ~globals ~locals
        ((assignments, heap): literal_memory) =
      ignore globals;
      let locals, globals =
        List.partition begin
          let local_names
            = Strings.of_list @@ List.rev_map Sc_C.Defs.var_name locals in
          fun { var; _ } -> Strings.mem var local_names
        end assignments
      in
      {
        pp_heap = begin fun ~static ppf ->
          print_literal_heap ~static ppf heap
        end;
        pp_globals = begin fun ppf ->
          pp_instrs (pp_assignment ~declare:false) ppf globals
        end;
        pp_locals = begin fun ppf ->
          pp_instrs (pp_assignment ~declare:true) ppf locals
        end;
      }

    let instructions_as_c_code ~globals ~locals (instructions: instructions) =
      ignore globals;
      let local_names = Strings.of_list @@ List.rev_map Sc_C.Defs.var_name locals in
      let locals, globals =
        List.partition begin function
          | Assignment { var; _ } -> Strings.mem var local_names
          | Alloc { ptr_ap; _ } -> Strings.mem (AP.origin' ptr_ap) local_names
        end instructions
      in
      {
        pp_heap = (fun ~static:_ _ppf -> ());
        pp_globals = begin fun ppf ->
          pp_instrs (pp_instruction ~declare:false) ppf globals
        end;
        pp_locals = begin fun ppf ->
          pp_instrs (pp_instruction ~declare:true) ppf locals
        end;
      }
  end
end

(* --- *)

let rec pp_literal_binding ppf = function
  | LBStr s -> Fmt.pf ppf "%S" s
  | LBMap m -> StrMap.print pp_literal_binding ppf m
  | LBArr a -> PPrt.pp_arr pp_literal_binding ppf a
  | LBLst l -> PPrt.pp_lst pp_literal_binding ppf l
  | LBNil -> Fmt.string ppf "(nil)"
  | LBRef b -> Fmt.pf ppf "&@[%a@]" pp_literal_binding b

let pp_field_access ppf fa =
  Fmt.pf ppf "{ ap_suffix: %a; }" Sc_C.Access_path.print_suffix fa.ap_suffix

let pp_assigned_info ppf (_v, Ctypes_static.BoxedType t) =
  Fmt.pf ppf "assignment@ of@ a@ memory@ location@ of@ type@ %a" format_typ' t

let pp_assignment_info ppf { binding; assigned } =
  Fmt.pf ppf "%a@ in@ %a" pp_literal_binding binding pp_assigned_info assigned

let pp_literal_binding_error ppf = function
  | Unexpected_literal { binding; msg;
                         assigned = Some (_v, BoxedType t) } ->
      Fmt.pf ppf "Invalid@ literal@ %a@ in@ assignment@ of@ a@ memory@ location@ \
                  of type@ %a:@;%s" pp_literal_binding binding format_typ' t
        (String.uncapitalize_ascii msg)
  | Unexpected_literal { binding; msg; assigned = None } ->
      Fmt.pf ppf "Invalid@ literal@ %a@ in@ assignment:@ %s\
                 " pp_literal_binding binding
        (String.uncapitalize_ascii msg)
  | Unsupported_assignment assignment ->
      Fmt.pf ppf "Unsupported@ literal@ binding@ %a\
                 " pp_assignment_info assignment
  | Unknown_enum { enumname; assignment } ->
      Fmt.pf ppf "Unknown@ enum@ type@ %s@ of@ literal@ binding@ %a@]\
                 " enumname pp_assignment_info assignment
  | Incompatible_array_length { expected_len; rvalue } ->
      Fmt.pf ppf "Incompatible@ length@ in@ literal@ bindings:@ %d@ (given)@ !=@ \
                  %d@ (expected)"
        (Array.length rvalue) expected_len
  | Bad_array_literal_length { max_len; given_len; assignment } ->
      Fmt.pf ppf "Error@ in@ literal@ binding@ %a:@;length@ of@ array@ literal@ \
                  exceeds@ internal@ upper@ bound:@;@ %u@ items@ given;@ upper@ \
                  limit@ is@ %u" pp_assignment_info assignment given_len max_len
  | Bad_cstring_literal_length { string = s; params; assignment } ->
      Fmt.pf ppf "Error@ in@ literal@ binding@ %a:@;length@ of@ string@ literal@ \
                  exceeds@ internal@ upper@ bound:@ string@ `%S'@ has@ %d@ \
                  characters;@ upper@ limit@ is@ %u" pp_assignment_info assignment
        s (String.length s) params.max_cstring_length

module LBDecoding = struct

  let lb_error e =
    raise @@ LITERAL_BINDING_ERROR e

  let unexpected binding ~expected =
    let report ppf =
      PPrt.string_to
        (fun msg -> lb_error @@ Unexpected_literal { binding; assigned = None; msg })
        (ppf^^"@ (expected@ "^^expected^^")")
    in
    match binding with
    | LBStr s -> report "Unexpected literal `%S' in bindings" s
    | LBMap _ -> report "Unexpected named mapping in literal binding"
    | LBArr _ -> report "Unexpected array in literal binding"
    | LBLst _ -> report "Unexpected list in literal binding"
    | LBNil   -> report "Unexpected (nil) in literal binding"
    | LBRef _ -> report "Unexpected reference in literal binding"

  (* exception InvalidStringLiteral of literal_binding list * string *)

  (* let string_of_chars: literal_binding list -> string = fun lits -> *)
  (*   let invalid_strlit ppf = *)
  (*     kasprintf (fun s -> raise @@ InvalidStringLiteral (lits, s)) ppf *)
  (*   in *)
  (*   let rec str acc = function *)
  (*     | LBStr "\000" :: _ -> *)
  (*         let chars = Array.of_list @@ List.rev acc in *)
  (*         Bytes.init (Array.length chars) (Array.get chars) |> Bytes.to_string *)
  (*     | LBStr s :: tl when String.length s = 1 -> *)
  (*         str (s.[0] :: acc) tl *)
  (*     | LBStr s :: _ -> *)
  (*         invalid_strlit "'%S' is not a valid character" s *)
  (*     | l :: _ -> *)
  (*         invalid_strlit "Unexpected literal '%a'" pp_literal_binding l *)
  (*     | [] -> *)
  (*         invalid_strlit "'\\000' not found in character sequence" *)
  (*   in *)
  (*   str [] lits *)

  let ( ~$ ) = function LBStr s -> s
                      | b -> unexpected b ~expected:"scalar"
  and ( ~^ ) = function LBStr s -> `Lit s | LBNil -> `Nil
                      | LBArr a -> `Arr a | LBLst l -> `Arr (Array.of_list l)
                      | b -> unexpected b ~expected:"nil, string literal, or \
                                                     collection of characters"
  and ( ~% ) = function LBMap s -> s
                      | b -> unexpected b ~expected:"named mapping"
  and ( ~@ ) = function LBArr a -> a | LBLst l -> Array.of_list l
                      | b -> unexpected b ~expected:"array or list"
  and ( ~@*) = function LBArr a -> a | LBLst l -> Array.of_list l
                      | LBRef s -> [|s|]
                      | b -> unexpected b ~expected:"array, list or ref"
  and ( ~* ) = function LBNil -> None
                      | LBRef b | (LBStr _ | LBArr _ | LBLst _ as b) -> Some b
                      | b -> unexpected b ~expected:"pointer"

end

(* TODO: check handling of complex literals *)
(** [apply_literal_bindings (module Encoding) typdecls ~h p lb] mutates the C
    value referenced by a {i non-null} pointer [p] according to literal bindings
    [lb], and returns the heap representation associated with the mutated value
    (for book-keeping of nested C pointers). *)
let apply_literal_bindings (module Encoding: LOW_LEVEL_ENCODING) typdecls ~h =
  let open Ctypes in
  let open Scanf in
  let open Signed in
  let open Unsigned in
  let open LBDecoding in
  let complex re im = Complex.{ re; im }
  and complexl re im =
    ComplexL.make (LDouble.of_string re) (LDouble.of_string im)
  in
  let h = ref h in
  let assigned_info p =
    let open Printer.Values (Encoding) in
    let pp_val ppf p = pp_pseudo_literal (reference_type p) ~h:!h ppf !@p in
    Fmt.str "%a" pp_val p, Ctypes_static.BoxedType (reference_type p)
  in
  let literal_length: literal_binding -> int = function
    | LBStr _ | LBMap _ -> 1
    | LBLst l -> List.length l
    | LBArr a -> Array.length a
    | LBNil | LBRef _ -> 1
  and alloc_blank: type a. a typ -> int -> _ -> a ptr = fun t len kind ->
    let p = allocate_n t ~count:len in h := Heap.add p len kind !h; p
  in
  let scan_c b =
    try
      let s = ~$b in
      if String.length s > 1 && s.[0] = '\''
      then sscanf s "%C" Fun.id
      else sscanf s "%d" (fun d -> Char.chr @@ if d >= 0 then d else 256 + d)
    with Scan_failure _ | Invalid_argument _ ->
      unexpected b ~expected:"character"
  and scan_bool b = match ~$b with
    | "0" | "false" | "FALSE" -> false
    | "1" | "true"  | "TRUE" -> true
    | _ -> unexpected b ~expected:"Boolean"
  and scan_stdbool b = match ~$b with
    | "0" | "false" | "FALSE" -> UChar.zero
    | "1" | "true"  | "TRUE" -> UChar.one
    | s when String.starts_with ~prefix:"0b" s -> UChar.of_string s
    | _ -> unexpected b ~expected:"Boolean"
  in
  let scan_digits ~s f b = sscanf ~$b ("%[-+0-9]" ^^ s) (fun d -> f d)
  and scan_float ~s f b = sscanf ~$b ("%[-+0-9.eE]" ^^ s) (fun d -> f d) in
  let scan_u f = scan_digits ~s:"%_[uU]" f
  and scan_l f = scan_digits ~s:"%_[lL]" f
  and scan_ul f = scan_digits ~s:"%_[uUlL]" f
  and scan_f f = scan_float ~s:"%_[fF]" f
  and scan_fl f = scan_float ~s:"%_[fFlL]" f in
  let open Encoding in
  let params = Encoding.Params.encoding_params in
  let rec aux: type a. a ptr -> _ -> literal_binding -> unit = fun p p_attrs b ->
    let { depth; _ } = p_attrs in
    assert (not (is_null p));
    try match reference_type p with
      | Void -> ()
      | Primitive Char -> p <-@ scan_c b
      | Primitive Schar -> sscanf ~$b "%c" (fun c -> p <-@ Char.code c) (* CHECKME *)
      | Primitive Uchar -> p <-@ scan_u UChar.of_string b
      | Primitive Bool -> p <-@ scan_bool b
      | Primitive Short -> p <-@ Int.of_string ~$b
      | Primitive Int -> p <-@ Int.of_string ~$b                 (* OCaml int? *)
      | Primitive Long -> p <-@ scan_l Long.of_string b
      | Primitive Llong -> p <-@ scan_l LLong.of_string b
      | Primitive Ushort -> p <-@ scan_u UShort.of_string b
      | Primitive Sint -> p <-@ SInt.of_string ~$b
      | Primitive Uint -> p <-@ scan_u UInt.of_string b
      | Primitive Ulong -> p <-@ scan_u ULong.of_string b
      | Primitive Ullong -> p <-@ scan_ul ULLong.of_string b
      | Primitive Size_t -> p <-@ Size_t.of_string ~$b
      | Primitive Int8_t -> p <-@ Int.of_string ~$b
      | Primitive Int16_t -> p <-@ Int.of_string ~$b
      | Primitive Int32_t -> p <-@ Int32.of_string ~$b
      | Primitive Int64_t -> p <-@ Int64.of_string ~$b
      | Primitive Uint8_t -> p <-@ scan_u UInt8.of_string b
      | Primitive Uint16_t -> p <-@ scan_u UInt16.of_string b
      | Primitive Uint32_t -> p <-@ scan_u UInt32.of_string b
      | Primitive Uint64_t -> p <-@ scan_ul UInt64.of_string b
      | Primitive Camlint -> p <-@ Int.of_string ~$b
      | Primitive Nativeint -> p <-@ Nativeint.of_string ~$b
      | Primitive Float -> p <-@ scan_f Float.of_string b
      | Primitive Double -> p <-@ scan_f Float.of_string b
      | Primitive LDouble -> p <-@ scan_fl LDouble.of_string b
      | Primitive Complex32 -> p <-@ sscanf ~$b "%f %+f*I" complex   (* "*I" *)
      | Primitive Complex64 -> p <-@ sscanf ~$b "%f %+f*I" complex   (* or "i" *)
      | Primitive Complexld -> p <-@ sscanf ~$b "%[+-0-9.eE]@ %[+-0-9.eE]\
                                                " complexl
      | Struct { fields; _ } ->
          let map = ~%b in
          List.iter begin fun (Ctypes_static.BoxedField f) ->
            match StrMap.find_opt (field_name f) map with
            | Some v ->
                let p_attrs = ptr_attrs ~pv:(ptr_field_validity f) depth in
                aux (p |-> f) p_attrs v
            | None -> ()
          end fields
      | Union { ufields; _ } ->
          let map = ~%b in
          (* TODO: use filter_map later to detect multiple bindings when we can
             restrict to OCaml >= 4.11 *)
          ignore @@ List.exists begin fun (Ctypes_static.BoxedField f) ->
            match StrMap.find_opt (field_name f) map with
            | Some v ->
                let p_attrs = ptr_attrs ~pv:(ptr_field_validity f) depth in
                aux (p |-> f) p_attrs v; true
            | None -> false
          end ufields
      | Array (_typ, len) ->
          let arr = if len = 1 then ~@*b else ~@b  (* allow LBRef if len = 1 *)
          and start_ptr = CArray.start !@p in
          if Array.length arr > len
          then lb_error @@ Incompatible_array_length { expected_len = len;
                                                       rvalue = arr };
          let p_attrs = ptr_attrs depth in
          Array.iteri (fun i -> aux (start_ptr +@ i) p_attrs) arr
      | Pointer typ ->
          pointer typ p p_attrs b
      | View _ as t when Boxed_typ t == Boxed_typ stdbool ->
          coerce (ptr t) (ptr stdbool) p <-@ scan_stdbool b
      | View _ ->
          named_enum p b
      | _ ->
          lb_error @@ Unsupported_assignment { assigned = assigned_info p;
                                               binding = b }
    with
    | Failure msg ->
        lb_error @@ Unexpected_literal { binding = b; msg;
                                         assigned = Some (assigned_info p) }
    | LITERAL_BINDING_ERROR Unexpected_literal ({ assigned = None; _ } as e) ->
        lb_error @@ Unexpected_literal { e with
                                         assigned = Some (assigned_info p) }
  and pointer: type a. a typ -> a ptr ptr -> _ = fun typ p p_attrs b ->
    match p_attrs with     (* TODO: check depth against max_non_nil_ptr_depth *)
    | { pv = `Carray_with_bound_length _ as pv; depth } ->
        carray typ p pv depth b
    | { pv = `Carray_with_length_field _; depth } ->
        carray typ p
          (`Carray_with_bound_length params.max_ptr_array_length) depth b
    | { pv = `Cstring; _ } ->
        cstring typ p b
  and carray: type a. a typ -> a ptr ptr -> _ =
    fun typ p (`Carray_with_bound_length ub) depth b ->
      let curlen = Heap.ptr_view !h !@p in
      match ~*b with
      | None ->                                            (* assign with NULL *)
          h := release_ptr !@p !h;
          p <-@ from_voidp typ null
      | Some b ->
          let len = literal_length b in
          if curlen <> BLCK len
          then begin
            h := release_ptr !@p !h;
            p <-@ alloc_blank typ len `naked
          end;
          if len > ub
          then bad_ptr_array_length len ~max:ub (assigned_info p) b;
          if len = 0 then () else                                     (* skip *)
            let p_attrs = ptr_attrs @@ succ depth in
            if len = 1
            then aux !@p p_attrs b
            else aux (coerce (ptr typ) (ptr (array len typ)) !@p) p_attrs b
  and cstring: type a. a typ -> a ptr ptr -> literal_binding -> unit = fun typ p b ->
    let max_len = params.max_cstring_length in
    h := release_ptr !@p !h;
    match ~^b with
    | `Nil ->
        p <-@ from_voidp typ null
    | `Lit s ->
        let len = String.length s in
        if len > max_len
        then bad_cstring_length s (assigned_info p) b;
        (* TODO should we assume double quotes here? (to decide, let's see how
           CBMC string literals look like). *)
        let ptr =
          (* Invariant: char*'s must either be NULL, or refer to a char carray
             of exactly max_cstring_length elements. *)
          (* So here we're padding with zeros. *)
          let padding_len = max_len - len in
          let s = s ^ String.make padding_len '\000' in
          CArray.start @@ str_coercion (array max_len typ) s
        in
        h := Heap.add ptr max_len `cstring !h;
        p <-@ ptr
    | `Arr a ->
        (* interpret as concatenation of char code literals, ignore depth *)
        let len = Array.length a in
        if len > max_len
        then bad_ptr_array_length len ~max:max_len (assigned_info p) b;
        let p' = alloc_blank char max_len `cstring in
        Array.iteri (fun i bi -> p' +@ i <-@ scan_c bi) a;
        h := Heap.add p' max_len `cstring !h;
        p <-@ coerce (ptr char) (ptr typ) p';
  and named_enum: type a. a ptr -> literal_binding -> _ = fun p b ->
    let t = reference_type p in
    let typname = Fmt.str "%a" format_typ' t in
    match
      let enum_typname = Scanf.sscanf typname "enum %s" (fun s -> s) in
      StrMap.find enum_typname typdecls
    with
    | CEnum v ->
        (coerce (ptr t) (ptr (View v)) p) <-@ ~$b
    | _
    | exception Scan_failure _
    | exception Not_found ->
        lb_error @@ Unknown_enum { enumname = typname;
                                   assignment = { assigned = assigned_info p;
                                                  binding = b } }
  and bad_ptr_array_length ~max:max_len given_len assigned binding =
    lb_error @@ Bad_array_literal_length { max_len; given_len;
                                           assignment = { assigned; binding } }
  and bad_cstring_length string assigned binding =
    lb_error @@ Bad_cstring_literal_length { string; params;
                                             assignment = { assigned; binding } }
  in
  fun p lb ->
    aux p (ptr_attrs 0) lb;
    !h

(* --- *)

module type VAL = sig
  module Printer: VALUE_PRINTER
  type t
  type typ
  val blank: typ -> t
  val of_string: typ -> string -> t
  val to_string: t -> string
  val read: typ -> in_channel -> t
  val write: out_channel -> t -> unit
  val typ: t -> typ
  val print: t PPrt.pp
  val as_c_literal: ?lit_heap:(Printer.literal_heap as 'm) -> t -> string * 'm
  val assign_from_literal: typdecls -> t -> literal_binding -> unit
  val fold_pointers: deep:bool -> f:('a -> 'a) cptr_consumer -> t -> 'a -> 'a
end

module type REPR = sig
  module Params: ENCODING_PARAMS
  type typ
  val size_bounds: typ -> int * int
  val reachable_types_with_pointers: typ -> Ctype_set.t
  val fold_access_paths
    : typ
    -> ?max_depth: int
    -> ?f_prm:'a access_path_folder
    -> ?f_ptr:'a ptr_access_path_folder
    -> 'a -> 'a
  val fold_direct_access_paths
    : typ
    -> ?f_prm:'a access_path_folder
    -> ?f_ptr:'a ptr_access_path_folder
    -> 'a -> 'a
  val fold_reachable_pointers
    : typ
    -> ?max_depth: int
    -> f:'a ptr_attributes_folder
    -> merge:('a NEL.t -> 'a)
    -> 'a -> 'a
  val visit_access_paths
    : typ
    -> ?max_depth: int
    -> ?f_prm:'a access_path_visitor
    -> ?f_ptr:'a ptr_access_path_visitor
    -> 'a -> 'a
  module Val: VAL with type typ := typ
end

module type AGGREG_TYP = sig
  type typ
  type value
  val size: typ -> int
  val definition: typ PPrt.pp
  val print: typ PPrt.pp
  val declare: string -> typ -> typdecls -> typdecls
  (* val as_any: typ -> any_ ctyp *)
  val as_ctyp: typ -> f:'a ctyp_consumer -> 'a
  exception Invalid_value_representation of typ * string option
  module type REPR =
    REPR with type typ := typ
  module Repr (Params: ENCODING_PARAMS):
    REPR with module Params = Params
end

module type C_LAYOUT = sig
  type k
  type value
  val up: k ctyp -> any_ ctyp
  module MakeVal (_: LOW_LEVEL_ENCODING) : sig
    module Printer: VALUE_PRINTER
    val print: k ctyp -> h:heap -> value PPrt.pp
    val as_c_literal
      : k ctyp
      -> ?lit_heap:(Printer.literal_heap as 'm)
      -> h:heap
      -> value
      -> string * 'm
    val assign_literal
      : k ctyp
      -> typdecls
      -> h:heap
      -> value
      -> literal_binding
      -> heap
    val read_value: k ctyp -> (int -> string) -> value * heap
    val write_value: k ctyp -> (string -> unit) -> h:heap -> value -> unit
    val fold_pointers
      : k ctyp
      -> deep:bool
      -> f:('a -> 'a) cptr_consumer
      -> h:heap
      -> value
      -> 'a -> 'a
    val blank: k ctyp -> value
  end
end

module Aggreg (CLayout: C_LAYOUT) = struct
  include CLayout
  type typ = k ctyp

  let declare name t = declare_typ name (up t)

  let size typ = typ_size (up typ)
  let definition ppf typ = print_def ppf (up typ)
  let print ppf typ = print_typ ppf (up typ)
  (* let as_any = up *)
  let as_ctyp typ = as_ctyp (up typ)

  (* That one cannot at the moment be put into `Exceptions' due to cyclic
     dependencies; for now it is simpler to leave it here: *)
  exception Invalid_value_representation of typ * string option

  ;;
  Printexc.register_printer (function
      | Invalid_value_representation (typ, s) ->
          PPrt.string_to Option.some
            "Invalid representation for value of type %a%a"
            print typ Fmt.(option @@ fmt ":@ %S") s
      | _ -> None)
  ;;

  let string_reader s =
    let r = ref s in
    (fun i ->
       if i > String.length !r then raise Exit;
       let s = Str.string_before !r i
       and s' = Str.string_after !r i in
       r := s';
       s), r

  let string_writer () =
    let r = ref "" in (fun s -> r := !r ^ s), r

  (* --- *)

  module type REPR =
    REPR with type typ := typ

  module Repr (Params: ENCODING_PARAMS) = struct
    module Params = Params
    module Encoding = Encoding (Params)
    module Printer = Printer.Values (Encoding)

    let size_bounds typ =
      Encoding.typ_size_bounds (up typ)

    let fold_direct_access_paths typ =
      as_ctyp typ ~f:{ f = Encoding.ctyp_fold_direct_access_paths }

    let fold_access_paths typ =
      as_ctyp typ ~f:{ f = Encoding.ctyp_fold_access_paths }

    let fold_reachable_pointers typ =
      as_ctyp typ ~f:{ f = Encoding.ctyp_fold_reachable_pointers }

    let visit_access_paths typ =
      as_ctyp typ ~f:{ f = Encoding.ctyp_visit_access_paths }

    let fold_pointed_typs typ ~f acc =
      let open Ctypes_static in
      let rec aux: type k. k Ctypes.typ -> 'a -> 'a =
        fun t ((visited, acc') as acc) ->
          match t with
          | Pointer t' ->
              let k = Ctype_key.of_typ t' in
              if Ctype_set.mem k visited
              then acc
              else aux t' (Ctype_set.add k visited, f.f t' acc')
          | Array (t', _) ->
              aux t' acc
          | Struct s ->
              visit_fields s.fields acc
          | Union u ->
              visit_fields u.ufields acc
          | _ ->
              acc
      and visit_fields: type s. s boxed_field list -> _ = fun fields acc ->
        List.fold_left (fun acc (BoxedField f) -> aux f.ftype acc) acc fields
      in
      snd @@ as_ctyp typ ~f:{ f = fun t -> aux t (Ctype_set.empty, acc) }

    let reachable_types_with_pointers typ =
      fold_pointed_typs typ Ctype_set.empty ~f:{
        f = fun (type k) (t: k Ctypes.typ) acc ->
          if has_pointers t
          then Ctype_set.add (Ctype_key.of_typ t) acc
          else acc
      }

    (* --- *)

    module Val = struct
      module Encoding = Encoding
      module Printer = Printer
      module C = MakeVal (Encoding)

      let value_of_string typ s =
        try let reader, _r = string_reader s in C.read_value typ reader
        with Exit ->
          raise @@ Invalid_value_representation (typ, Some s)

      let string_of_value typ ~h v =
        let writer, r = string_writer () in
        C.write_value typ writer ~h v;
        !r

      let input_value typ ic =
        try C.read_value typ @@ really_input_string ic
        with End_of_file | Exit ->
          raise @@ Invalid_value_representation (typ, None)

      let output_value oc typ =
        C.write_value typ (output_string oc)

      (* --- *)

      type t = { typ : typ; value: value; mutable heap: heap }
      let typ v = v.typ
      let blank typ = { typ; value = C.blank typ; heap =  Heap.empty }
      let _input f typ x = let value, heap = f typ x in { typ; value; heap }
      let _output f v = f v.typ ~h:v.heap v.value
      let of_string = _input value_of_string
      let to_string = _output string_of_value
      let read = _input input_value
      let write oc = _output (output_value oc)
      let print ppf = _output (fun typ -> C.print typ ppf)
      let as_c_literal ?lit_heap = _output (C.as_c_literal ?lit_heap)
      let assign_from_literal dcls v lb =
        v.heap <- C.assign_literal v.typ dcls ~h:v.heap v.value lb
      let fold_pointers ~deep ~f v =
        C.fold_pointers v.typ ~deep ~h:v.heap ~f v.value
    end
  end
end

module type NAMED_MAPPING = sig
  include AGGREG_TYP
  val from_cil_fields
    : ?typdecls:typdecls
    -> string
    -> Sc_C.Types.vars
    -> typ
end

module NamedMapping
    (CLayout: sig
       type k
       type value
       val up: k ctyp -> any_ ctyp
       val ctyp: k ctyp -> value Ctypes.typ
       val addr: value -> value Ctypes.ptr
       val fields: k ctyp -> value Ctypes_static.boxed_field list
     end) =
struct
  include Aggreg (struct
      open Ctypes
      include CLayout
      module MakeVal (Encoding: LOW_LEVEL_ENCODING) = struct
        module Printer = Printer.Values (Encoding)
        let print typ = Printer.pp_pseudo_literal (ctyp typ)
        let as_c_literal typ = Printer.as_c_literal (ctyp typ)
        let assign_literal _typ typdecls ~h v =
          apply_literal_bindings (module Encoding) typdecls ~h (addr v)
        let read_value typ = Encoding.read_value (ctyp typ)
        let write_value typ = Encoding.write_value (ctyp typ)
        let fold_pointers typ ~deep ~f ~h =
          Encoding.cval_fold_pointers (ctyp typ) ~f ~h ~deep
        let blank typ = !@(allocate_n (ctyp typ) ~count:1)
      end
    end)

  let fold_fields typ ~f acc =
    List.fold_left begin fun acc (Ctypes_static.BoxedField fld) ->
      f.f fld.ftype fld.fname acc
    end acc (CLayout.fields typ)

  module Repr (Params: ENCODING_PARAMS) = struct
    include Repr (Params)

    let fold_fields typ
        ?(f_prm = { f = fun _ _ acc -> acc })
        ?(f_ptr = { f = fun _ _ _ acc -> acc })
        acc =
      let module AP = Sc_C.Access_path in
      List.fold_left begin fun acc (Ctypes_static.BoxedField f) ->
        match f.ftype with
        | Pointer t ->
            let pv = Encoding.memory_validity @@ ptr_field_validity f in
            f_ptr.f t f.fname pv acc
        | t ->
            f_prm.f t f.fname acc
      end acc (CLayout.fields typ)

  end
end

let def_from_cil_fields ?(typdecls = empty_typdecls) t = function
  | [] -> t                       (* do not seal composed types with no fields *)
  | fields ->
      let delayed_ops = List.map (fun f -> add_field typdecls t f) fields in
      Ctypes.seal t;
      List.iter Lazy.force delayed_ops;
      t

module Struct: sig
  include NAMED_MAPPING with type typ = struct_ ctyp
                         and type value = cside Ctypes.structure
  val padding_bytes: typ -> int list
  val bytes_layout: typ -> (value Ctypes_static.boxed_field * int) list
  val fold_fields: typ -> f:'a field_folder -> 'a -> 'a
  module type VAL = sig
    include VAL with type typ := typ

    (** [fields_as_c_literals v] returns a pair [(fields, heap)]: [fields] is a
        list of pairs of strings [(fname, fval)], where [fname] is the name of a
        field in the type of [v], and [fval] is a human-readable, C-parseable,
        representation of its value in [v]; in turn, [heap] is a possibly empty
        literal heap.  *)
    val fields_as_c_literals: t -> Printer.literal_memory

    (** [fields_as_c_allocations v] returns a list of instructions that
        initializes one variable [f] for each field [f] of [v].  Each heap
        object pointed to via a field that is (transitively) reachable from any
        [f] is dynamically allocated.  *)
    val fields_as_c_allocations: t -> Printer.instructions
  end
  module type REPR = sig
    include REPR
    val fold_fields
      : typ
      -> ?f_prm:'a field_folder
      -> ?f_ptr:'a ptr_field_folder
      -> 'a -> 'a
    module Val: VAL
  end
  module Repr (Params: ENCODING_PARAMS):
    REPR with module Params = Params
end = struct

  include NamedMapping (struct
      type k = struct_
      type ktyp = k ctyp
      type value = cside Ctypes.structure
      let up (CStruct _ as t: ktyp) = (t :> any_ ctyp)
      let ctyp (CStruct s: ktyp) = Ctypes_static.Struct s
      let addr = Ctypes.addr
      let fields (CStruct { fields; _ }: ktyp) = fields
    end)

  let bytes_layout (CStruct s : struct_ ctyp) =
    List.combine s.fields (padding_bytes s)

  let padding_bytes (CStruct s : struct_ ctyp) =
    padding_bytes s

  module type VAL = sig
    include VAL with type typ := typ
    val fields_as_c_literals: t -> Printer.literal_memory
    val fields_as_c_allocations: t -> Printer.instructions
  end

  module type REPR = sig
    include REPR
    val fold_fields
      : typ
      -> ?f_prm:'a field_folder
      -> ?f_ptr:'a ptr_field_folder
      -> 'a -> 'a
    module Val: VAL
  end

  module Repr (Params: ENCODING_PARAMS) = struct
    include Repr (Params)

    module Val = struct
      include Val
      let fields_as_c_literals { typ = (CStruct t); value; heap } =
        Printer.fields_as_c_literal_assignments t value heap
      let fields_as_c_allocations { typ = (CStruct t); value; heap } =
        Printer.fields_as_c_allocations t value heap
    end
  end

  let from_cil_fields ?(typdecls = empty_typdecls) name fields : typ =
    match
      match StrMap.find_opt name typdecls with
      | None -> Ctypes.structure name
      | Some (CStruct s) -> Struct s
      | Some _ -> Fmt.failwith "Invalid declaration of struct %s: name is already \
                                bound" name
    with
    | Struct s as t ->
        let typdecls = declare_typ name (CStruct s) typdecls in
        ignore @@ def_from_cil_fields ~typdecls t fields;
        CStruct s
    | _ -> failwith "Wrong internal type built"
end

module Union: NAMED_MAPPING with type typ = union_ ctyp
                             and type value = cside Ctypes.union =
struct
  include NamedMapping (struct
      type k = union_
      type ktyp = k ctyp
      type value = cside Ctypes.union
      let up (CUnion _ as t: ktyp) = (t :> any_ ctyp)
      let ctyp (CUnion u: ktyp) = Ctypes_static.Union u
      let addr = Ctypes.addr
      let fields (CUnion { ufields; _ }: ktyp) = ufields
    end)

  let from_cil_fields ?(typdecls = empty_typdecls) name fields : typ =
    match
      match StrMap.find_opt name typdecls with
      | None -> Ctypes.union name
      | Some (CUnion s) -> Union s
      | Some _ -> Fmt.failwith "Invalid declaration of union %s: name is already \
                                bound" name
    with
    | Union u as t ->
        let typdecls = declare_typ name (CUnion u) typdecls in
        ignore @@ def_from_cil_fields ~typdecls t fields;
        CUnion u
    | _ -> failwith "Wrong internal type built"
end

module BoxedArray: sig
  include AGGREG_TYP with type typ = array_ ctyp
  val from_cil_elt_typ
    : ?typdecls:typdecls -> Cil.typ -> int -> typ
end = struct
  open Ctypes

  include Aggreg (struct
      type k = array_
      type ktyp = k ctyp
      type value = BoxedArray: 'a carray -> value

      let up (CArray (typ, len): ktyp) = (CArray (typ, len) :> any_ ctyp)

      type 'a atyp = 'a carray Ctypes.typ
      type 'a aopen = { s: 'k. 'k atyp -> 'k carray -> 'a }
      let on_array f (BoxedArray v) =
        f.s (Array (CArray.element_type v, CArray.length v)) v

      module MakeVal (Encoding: LOW_LEVEL_ENCODING) = struct
        module Printer = Printer.Values (Encoding)
        let print (_: ktyp) ~h ppf =
          on_array { s = fun t -> Printer.pp_pseudo_literal t ~h ppf }

        let as_c_literal (_: ktyp) ?lit_heap ~h =
          on_array { s = fun t -> Printer.as_c_literal t ?lit_heap ~h }

        let assign_literal (_: ktyp) typdecls ~h =
          (* TODO: see if this trick would allow to export a more generic `val
             addr: value -> value ptr`, or `val addr: value -> _ naked_value_ptr`
             here. *)
          on_array { s = fun t v ->
              CArray.start v
              |> coerce (ptr (CArray.element_type v)) (ptr t)
              |> apply_literal_bindings (module Encoding) typdecls ~h }

        let read_value (CArray (typ, len): ktyp) r =
          let v, h = Encoding.read_value (Array (typ, len)) r in
          BoxedArray (v), h

        let write_value (CArray _: ktyp) r ~h =
          on_array { s = fun t -> Encoding.write_value t r ~h }

        let fold_pointers (CArray _: ktyp) ~deep ~f ~h =
          on_array { s = fun t -> Encoding.cval_fold_pointers ~deep ~f ~h t }

        let blank (CArray (typ, len): ktyp) =
          let p = allocate_n (Array (typ, len)) ~count:1 in
          BoxedArray !@p
      end
    end)

  let from_cil_elt_typ ?(typdecls = empty_typdecls) ty size : typ =
    ctyp typdecls { f = fun t -> CArray (t, size) } ty
end

(* --- *)

let enum_typ typdecls Cil.{ ename; eitems; ekind; _ } =
  let module IMap = Map.Make (Int64) in
  let intexp exp = match Cil.getInteger exp with
    | Some v when Cilint.is_int_cilint v ->
        let v = Cilint.int_of_cilint v in
        Int64.of_int v
    | _ ->
        Fmt.failwith "Invalid enumeration constant: %a"
          Sc_C.Printer.pp_exp exp
  in
  let repr2item, item2repr =
    List.fold_left begin fun (r2i, i2r) (item, _attributes, exp, _location) ->
      let v = intexp exp in
      IMap.add v item r2i, StrMap.add item v i2r
    end (IMap.empty, StrMap.empty) eitems
  in
  let view = ctyp typdecls { f = fun (type r) (repr_typ: r Ctypes.typ) ->
      let (of_i64: int64 -> r), (to_i64: r -> int64) = match repr_typ with
        | Primitive Char -> (fun i -> i |> Int64.to_int |> Char.chr),
                            (fun c -> c |> Char.code |> Int64.of_int)
        | Primitive Uchar -> Unsigned.UChar.(of_int64, to_int64)
        | Primitive Uint -> Unsigned.UInt.(of_int64, to_int64)
        | Primitive Ushort -> Unsigned.UShort.(of_int64, to_int64)
        | Primitive Int -> Signed.Int.(of_int64, to_int64)
        | t ->
            Fmt.failwith "Unsupported type of representation %a for enumeratiom \
                          %s"
              format_typ' t ename
      in
      Ctypes.view repr_typ
        ~format_typ:(fun pp ppf -> Fmt.pf ppf "enum %s%t" ename pp)
        ~format:Fmt.string
        ~read:begin fun r ->
          to_i64 r |> fun r -> match IMap.find_opt r repr2item with
          | Some s -> s
          | None -> Int64.to_string r
        end
        ~write:begin fun i ->
          try of_i64 @@ match StrMap.find_opt i item2repr with
            | Some r -> r
            | None -> Int64.of_string i
          with Failure _ ->
            Fmt.failwith "Unknown item `%s' for enumeration %s" i ename
        end
    } (TInt (ekind, []))
  in
  match view with
  | Ctypes_static.View v -> CEnum v
  | _ -> failwith "Wrong internal type built"

let append_global_cil_declaration_to_typdecls (d: Cil.global) typdecls =
  let fields =
    List.map (fun Cil.{ fname; ftype; fattr; _ } -> ftype, fname, fattr)
  in
  try match d with
    | GCompTagDecl ({ cstruct; cname; _ }, _) when cstruct ->
        Struct.(declare cname (from_cil_fields ~typdecls cname []) typdecls)
    | GCompTag ({ cstruct; cname; cfields;_ }, _) when cstruct ->
        Struct.(declare cname (from_cil_fields ~typdecls cname @@
                               fields cfields)) typdecls
    | GCompTagDecl ({ cname; _ }, _) ->
        Union.(declare cname (from_cil_fields ~typdecls cname []) typdecls)
    | GCompTag ({ cname; cfields; _  }, _) ->
        Union.(declare cname (from_cil_fields ~typdecls cname @@
                              fields cfields) typdecls)
    | GEnumTagDecl ({ ename; _ } as enum, _)
    | GEnumTag ({ ename; _ } as enum, _) ->
        StrMap.add ename (enum_typ typdecls enum) typdecls
    | GType (_ , _)                                                    (* ??? *)
    | _ ->
        typdecls
  with SPECIFICATION_ERROR e ->
    Logs.warn (fun p -> p "%a" pp_specification_error e);
    typdecls

let append_global_cil_declarations_to_typdecls decls typdecls =
  List.fold_left begin fun typdecls d ->
    append_global_cil_declaration_to_typdecls d typdecls
  end typdecls decls

(* TODO: find out if and how array type declarations are represented in Cil. *)
let typdecls_from_cil_file cil =
  Sc_C.Defs.fold_typ_defs cil append_global_cil_declaration_to_typdecls
    empty_typdecls

(* --- *)

let () =
  Printexc.register_printer begin function
    | LITERAL_BINDING_ERROR error ->
        PPrt.string_to Option.some "%a" pp_literal_binding_error error
    | SPECIFICATION_ERROR error ->
        PPrt.string_to Option.some "%a" pp_specification_error error
    | _ ->
        None
  end
