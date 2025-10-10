(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** An interface for representing C values of any type. *)

module TYPES: sig

  (** Abstract type for any C-side value *)
  type cside

  (** Abstract representation of C types *)
  type _ ctyp

  (** Types of some C types *)
  type struct_
  type union_
  type array_

  (** Polymorphic function over internal C types *)
  type 'a ctyp_consumer = { f: 'k. 'k Ctypes.typ -> 'a }

  (** Type of typing declarations for the naming and definition of composed
      types. *)
  type typdecls

  type encoding_params =
    {
      max_ptr_array_length: int;
      (** Maximal size of C arrays directly reachable via pointers. *)

      max_non_nil_ptr_depth: int;
      (** Maximal depth at which pointers may be non-NULL, measured in number of
          pointer dereferences. *)

      max_cstring_length: int;                   (* excluding a trailing '\000' *)
      (** Maximal size of C strings, excluding any additional terminating
          '\000'. *)
    }

  (** View of pointer attributes, exposed during structure traversals below *)
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

  and struct_field

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

  (** Type of bindings to C values directly from C string literals; given to
      {!VAL.assign_from_literal} below. *)
  type literal_binding =
    | LBStr of string
    | LBMap of literal_binding Basics.StrMap.t
    | LBArr of literal_binding array
    | LBLst of literal_binding list
    | LBNil                                           (** NULL reference *)
    | LBRef of literal_binding                        (** reference to bindings *)

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

  (** Exception raised in assignments with literal bindings *)
  exception LITERAL_BINDING_ERROR of literal_binding_error

end
include module type of TYPES
  with type encoding_params = TYPES.encoding_params
   and type struct_field = TYPES.struct_field
   and type field_access = TYPES.field_access
   and type memory_validity = TYPES.memory_validity
   and type ptr_attributes = TYPES.ptr_attributes
   and type 'a ctyp_consumer = 'a TYPES.ctyp_consumer
   and type 'a ptr_attributes_folder = 'a TYPES.ptr_attributes_folder
   and type 'a ptr_access_path_folder = 'a TYPES.ptr_access_path_folder
   and type 'a cptr_consumer = 'a TYPES.cptr_consumer

(* --- *)

(** An immutable, empty set of type declarations. *)
val empty_typdecls: typdecls

(** Note: ignores anything other than type declarations *)
val append_global_cil_declaration_to_typdecls: Cil.global -> typdecls -> typdecls
val append_global_cil_declarations_to_typdecls: Cil.global list -> typdecls -> typdecls

(** [typdecls_from_cil_file cil] returns a set of typing declarations built from
    all global type definitions in [cil]. *)
val typdecls_from_cil_file: Cil.file -> typdecls

(** [sizeof_cil_typ] is the size of the given {!Cil. type, in bytes (equivalent
    to [sizeof(t)]), possibly given some type declarations. *)
val sizeof_cil_typ: ?typdecls:typdecls -> Cil.typ -> int

val struct_field_typ: struct_field -> Ctypes_static.boxed_typ

(* --- *)

(** Prints raw literal bindings (for debug) *)
val pp_literal_binding: literal_binding Fmt.t
val pp_field_access : TYPES.field_access Fmt.t

val pp_literal_binding_error: TYPES.literal_binding_error Fmt.t
val pp_specification_error: TYPES.specification_error Fmt.t

(* --- *)

(** Parameters for binary encoding *)
module type ENCODING_PARAMS = sig
  val encoding_params: encoding_params
end

(** High-level encoding specification.  This only gathers parameters as no
   further details about the encoding should be exported. *)
module type ENCODING = sig module Params: ENCODING_PARAMS end

(** The type of represented values, parameterized by encoding parameters. *)
module type VALUE_PRINTER = sig

  (** A literal memory consists of initialized typed variables, along with a
      literal heap *)
  type literal_memory = literal_assignments * literal_heap
  and literal_assignments
  and literal_heap
  and instructions

  (** C code used to declare and initialize C memory *)
  type c_code =
    {
      pp_heap: static:bool -> Basics.PPrt.pu;
      (** [pp_heap ~static ppf] uses [ppf] to print parseable C code that
          declares and initializes heap memory cells.  Initialized declarations
          (i.e, `typ cell = lit;') are used if [static = true]; calls to `memcpy`
          are used otherwise (i.e, `typ cell; (void) memcpy (&cell, ...);`).  *)

      pp_globals: Basics.PPrt.pu;
      (** prints assignment of already declared variables *)

      pp_locals: Basics.PPrt.pu;
      (** prints declarations and initializations for local variables *)
    }

  (** [literal_memory_as_c_code ~globals ~locals instructions] constructs
      printers that output the given literal memory as C code.  This codes does
      not make use of any dynamic memory.  In particular, it does not use
      `malloc` --- it may however use `memcpy` to initialize large objects. *)
  val literal_memory_as_c_code
    : globals: Sc_C.Types.vars
    -> locals: Sc_C.Types.vars
    -> literal_memory
    -> c_code

  (** [instructions_as_c_code ~globals ~locals instructions] constructs printers
      that output [instructions] in C.  This code makes use of `malloc` and
      potentially `memcpy` to allocate and initialize heap objects.  Note
      [pp_heap] is a no-op (as the code of [instructions] does not rely on any
      static declaration or initialization of heap objects). *)
  val instructions_as_c_code
    : globals: Sc_C.Types.vars
    -> locals: Sc_C.Types.vars
    -> instructions
    -> c_code
end

(** Helpers to print C types and variable declarations *)
module Printer: sig
  (** [c_typ ppf typ] prints a low-level `ctypes` type [typ] as parseable C
      code.  *)
  val c_typ: 'a Ctypes.typ Fmt.t

  (** [c_decl ppf (typ, v)] prints a low-level declaration of variable [v] as
     `ctypes` type [typ] as parseable C code.  *)
  val c_decl: ('a Ctypes.typ * string) Fmt.t

  (** [cil_typ ppf typ] prints a {!Cil. type [typ] as parseable C code.  *)
  val cil_typ: Cil.typ Fmt.t

  (** [cil_decl ppf (typ, v)] prints a low-level declaration of variable [v] as
      {!Cil} type [typ] as parseable C code.  *)
  val cil_decl: (Cil.typ * string) Fmt.t

  (** Functor for printing represented values. *)
  module Values (_: ENCODING): VALUE_PRINTER
end

(** Encoding-specific low-level routines *)
module Encoding (_: ENCODING_PARAMS): sig

  (** [pp_c_ptr_decode ppf (v, ub, d)] prints using [ppf] a C expression that
      computes a length of array encoded by a given {i unsigned long} variable
      [v], at a depth encoded by an {i unsigned int} [d]. The lower/upper bound
      for the length is [lb]/[ub]. *)
  val pp_c_ptr_decode: (string * string * string * string) Fmt.t

  (** [pp_c_constrained_carray_length ppf (v, size, ub, d)] prints using [ppf] a
      C expression that computes the actual a length of an array with "given"
      length [*v], at a depth encoded by an {i unsigned int} [d].  [ub] is the
      upper bound on for the length.

      The main purpose of the resulting expression is to take the [depth] into
      accound to for the length to zero when the depth exceeds
      [max_non_nil_ptr_depth]. *)
  val pp_c_constrained_carray_length: (string* string * string * string) Fmt.t

  (** [pp_cstring_buff_length_decode fmt (v, d)] prints using [fmt] a C
     expression that computes a length of C character string, including the
     trailing '\000', encoded by a given {i unsigned long} variable [v], at a
     depth encoded by an {i unsigned int} [d]. *)
  val pp_c_cstring_buff_length_decode: (string * string) Fmt.t

  (** [max_pointed_size typ pv] is the maximum size {b in bytes} of an array of
      elements of type [typ] that may be pointed to by a pointer with validity
      [pv]. *)
  val max_pointed_size: 'k Ctypes.typ -> memory_validity -> int

  (** [ctyp_fold_access_paths ctyp ~max_depth ~f_prm ~f_ptr acc] folds [f_prm]
      and [f_ptr] over every canonical access path suffix that is may be a valid
      memory access from a value of type [ctyp].  [f_ptr] is used for explicit
      pointers; [f_prm] for every other type of data (including plain arrays);
      pointer types are visited recursively (up to [max_depth], which is by
      default [params.max_non_nil_ptr_depth]).

      Note: use [max_depth=0] to prevent traversal of pointers. *)
  val ctyp_fold_access_paths
    : _ Ctypes_static.typ
    -> ?max_depth:int
    -> ?f_prm:'a access_path_folder
    -> ?f_ptr:'a ptr_access_path_folder
    -> 'a -> 'a

  (** [ctyp_fold_direct_access_paths ctyp ~f_prm ~f_ptr acc] folds [f_prm] and
      [f_ptr][f] over every field defined in type [ctyp]; referenced types are
      NOT visited recursively.

      This is a shorthand for [ctyp_fold_access_paths ~max_depth:0 ?f_prm
      ?f_ptr]. *)
  val ctyp_fold_direct_access_paths
    : _ Ctypes.typ
    -> ?f_prm:'a access_path_folder
    -> ?f_ptr:'a ptr_access_path_folder
    -> 'a -> 'a

  (** [ctyp_fold_reachable_pointers ctyp ~max_depth ~f ~merge acc] folds [f]
      over every pointer field defined in type [ctyp]; pointed types are visited
      recursively (up to and including [max_depth]), and results of accumulation
      over union fields are computed using [merge].  [max_depth] defaults to
      {!max_non_nil_ptr_depth} (given via encoding parameters). *)
  val ctyp_fold_reachable_pointers
    : _ Ctypes_static.typ
    -> ?max_depth: int
    -> f:'a ptr_attributes_folder
    -> merge:('a NEL.t -> 'a)
    -> 'a -> 'a

  (** [ctyp_visit_access_paths ctyp ~max_depth ~f_prm ~f_ptr acc] visits every
      (canonicalized) access path that may be valid on a value of type [ctyp];
      pointers are visited using [f_ptr], while every other type of value is
      visited using [f_prm].

      The value returned by these visiting functions on any path [ap] drives the
      traversal: when [Do_children_and_then (acc, f)] is returned, any type of
      memory cell that may be accessed by appending a field or array indexing or
      pointer access suffix to [ap] is visited recursively (unless [max_depth]
      is reached when traversing a pointer type) with the accumulated value
      [acc]; then the resulting accumulated value is passed to [f].
      [Do_children acc] is equivalent to [Do_children_and_then (acc, Fun.id)].
      When [Skip_children acc] is returned, the result of the traversal of [ap]
      is [acc]. *)
  val ctyp_visit_access_paths
    : _ Ctypes_static.typ
    -> ?max_depth: int
    -> ?f_prm:'a access_path_visitor
    -> ?f_ptr:'a ptr_access_path_visitor
    -> 'a -> 'a

end

(** Encapsulated typed values *)
module type VAL = sig
  module Printer: VALUE_PRINTER

  type t

  type typ

  (** [blank typ] returns a new value of type [typ] with all bytes set to null
     (zero). *)
  val blank: typ -> t

  (** [of_string typ str] interprets the string of characters [str] as a C
      structured value of type [typ].  Raises {!Invalid_value_representation} if
      [str] is not long-enough to hold such a value. *)
  val of_string: typ -> string -> t

  (** [to_string typ v] returns the C structured value [v], of type [typ], as
      a string of bytes encoded in a string. *)
  val to_string: t -> string

  (** [read typ ic] acts like {!of_string}, but reads bytes from the input
      channel [ic].  *)
  val read: typ -> in_channel -> t

  (** [write oc v] writes the binary representation of the C structured value
      [v] onto [oc]. *)
  val write: out_channel -> t -> unit

  (** [typ v] returns the type of [v]. *)
  val typ: t -> typ

  (** [print fmt v] prints the structured value [v] as a C-like literal using
     the formatter [fmt]; in the presence of pointers, this does not generate
     parseable C code.  *)
  val print: Format.formatter -> t -> unit

  (** [as_c_literal ?lit_heap v] constructs a parseable C literal representation
     for the value [v], along with a possibly empty set of object literals
     representing cells of dynamic memory. *)
  val as_c_literal: ?lit_heap:(Printer.literal_heap as 'm) -> t -> string * 'm

  (** [assign_from_literal v bindings] performs a deep assignent of [v] {i
     w.r.t} the bindings described using [bindings].  *)
  val assign_from_literal: typdecls -> t -> literal_binding -> unit

  val fold_pointers: deep:bool -> f:('a -> 'a) cptr_consumer -> t -> 'a -> 'a
end

(** Representation of typed values that is parametric in encoding parameters *)
module type REPR = sig

  (** Encoding parameters for the representation *)
  module Params: ENCODING_PARAMS

  (** Type of representable values *)
  type typ

  (** [size_bounds typ] returns a pair of the minimal structure size (i.e,
     assuming all pointers are NULL), and maximal size (i.e., the total memory
     footprint with pointers unfolded according to
     {!Encoding.max_ptr_array_length}) *)
  val size_bounds: (* typdecls ->  *)typ -> int * int

  val reachable_types_with_pointers: typ -> Sc_C.Type_collections.SET.t

  (** [fold_access_paths typ ~max_depth ~f_prm ~f_ptr acc] folds [f_prm] and
      [f_ptr] over every canonical access path suffix that may be a valid memory
      access from a value of type [typ] (cf
      {!Encoding.ctyp_fold_access_paths}). *)
  val fold_access_paths
    : typ
    -> ?max_depth: int
    -> ?f_prm:'a access_path_folder
    -> ?f_ptr:'a ptr_access_path_folder
    -> 'a -> 'a

  (** [fold_direct_access_paths typ ~f_prm ~f_ptr acc] folds [f_prm] and [f_ptr]
      over every field defined in type [typ]; referenced types are NOT visited
      recursively (cf {!Encoding.ctyp_fold_direct_access_paths}). *)
  val fold_direct_access_paths
    : typ
    -> ?f_prm:'a access_path_folder
    -> ?f_ptr:'a ptr_access_path_folder
    -> 'a -> 'a

  (** [fold_reachable_pointers ctyp ~max_depth ~f ~merge acc] folds [f] over
      every pointer field defined in type [ctyp]; pointed types are visited
      recursively (up to and including [max_depth]), and results of accumulation
      over union fields are computed using [merge].  [max_depth] defaults to
      {!max_non_nil_ptr_depth} (given via encoding parameters) (cf
      {!Encoding.ctyp_fold_reachable_pointers}). *)
  val fold_reachable_pointers
    : typ
    -> ?max_depth: int
    -> f:'a ptr_attributes_folder
    -> merge:('a NEL.t -> 'a)
    -> 'a -> 'a

  (** [visit_access_paths typ ~max_depth ~f_prm ~f_ptr acc] visits every
      canonical access path suffix that may be a valid memory access from a
      value of type [typ] using [f_prm] and [f_ptr] (cf
      {!Encoding.ctyp_visit_access_paths}). *)
  val visit_access_paths
    : typ
    -> ?max_depth: int
    -> ?f_prm:'a access_path_visitor
    -> ?f_ptr:'a ptr_access_path_visitor
    -> 'a -> 'a

  (** Typed values *)
  module Val: VAL with type typ := typ
end

(** Representation of C composed types (structure, union, arrays).  These can be
    used to interpret any sufficiently long OCaml character string as a memory
    representation of a value for such a type.  *)
module type AGGREG_TYP = sig

  (** C composed type. *)
  type typ

  (** C value built for the composed type. *)
  type value

  (** [size typ] returns the size in bytes of the memory representation for
     values of the composed type [typ]. *)
  val size: typ -> int

  (** [print fmt typ] prints the C definition of the composed type [typ] on the
     formatter [fmt]. *)
  val definition: Format.formatter -> typ -> unit
  val print: Format.formatter -> typ -> unit

  (** [declare name typ typdecls] declares a [typ] with name [name] into
      [typdecls].  Any previous binding of [name] in [typdecls] is erased.  *)
  val declare: string -> typ -> typdecls -> typdecls

  (** [as_ctyp typ f] applies [f] on the `ctypes` representation of [typ]. *)
  val as_ctyp: typ -> f:'a ctyp_consumer -> 'a

  (** Exception raised by {!Val.of_string} when it is given a string that is too
      short to hold a value of type [typ]. *)
  exception Invalid_value_representation of typ * string option

  (** Bound representation types *)
  module type REPR =
    REPR with type typ := typ

  (** Parametric representation of typed values *)
  module Repr (Params: ENCODING_PARAMS):
    REPR with module Params = Params
end

(** Representation of C structured types (structure or union).  These can be
    built directly from a list of CIL field descriptions.  *)
module type NAMED_MAPPING = sig
  include AGGREG_TYP

  (** [from_cil_fields ~typdecls name fields] defines a composed type based on a
      list of named CIL fields. *)
  val from_cil_fields
    : ?typdecls:typdecls
    -> string
    -> Sc_C.Types.vars
    -> typ
end

(** Declaration and representation of C struct types. *)
module Struct: sig
  include NAMED_MAPPING with type value = cside Ctypes.structure
                         and type typ = struct_ ctyp

  (** [padding_bytes typ] returns, for each field [f] of the given structure
      type (considered in order), the number of padding bytes that are placed {e
      right after} [f] for alignment purposes. *)
  val padding_bytes: typ -> int list
  val bytes_layout: typ -> (value Ctypes_static.boxed_field * int) list

  (** [fold_fields typ ~f acc] folds [f] over the fields of [typ]. *)
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

  (** Structure-specific functions *)
  module type REPR = sig
    include REPR

    (** [fold_fields typ ~f_prm ~f_ptr acc] folds [f_prm] and/or [f_ptr] over
        the fields of [typ]: [f_ptr] is given the type [t] for each pointer
        field of type [t*], whereas [f_prm] is called with type [t] for every
        other field of type [t]. *)
    val fold_fields
      : typ
      -> ?f_prm:'a field_folder
      -> ?f_ptr:'a ptr_field_folder
      -> 'a -> 'a

    module Val: VAL
  end

  (** Parametric representation of typed values *)
  module Repr (Params: ENCODING_PARAMS):
    REPR with module Params = Params
end

(** Declaration and representation of C unions. *)
module Union: NAMED_MAPPING with type value = cside Ctypes.union
                             and type typ = union_ ctyp

(** Declaration and representation of C arrays. *)
module BoxedArray: sig
  include AGGREG_TYP with type typ = array_ ctyp

  (** [from_cil_elt_typ ~typdecls typ size] defines an array type based on the
     CIL type [typ] for elements and a size [size]. *)
  val from_cil_elt_typ: ?typdecls:typdecls -> Cil.typ -> int -> typ
end

(* --- *)

(** {2 Other utilities for easy access to Ctypes} *)

(** [has_pointers t] is true iff the C type [t] is composed of at least one
   pointer field. *)
val has_pointers: 'k Ctypes.typ -> bool
