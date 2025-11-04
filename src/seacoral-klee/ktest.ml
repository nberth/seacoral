(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Handling of [.ktest] files *)

open Sc_sys.File.TYPES
open Harness.TYPES                                                 (* options *)

open Sc_sys.File.Syntax

type error =
  | Invalid_bout_header of string
  | Unsupported_version of int

exception ERROR of error
exception MALFORMED_RAW_TEST                                      (* internal *)

let pp_error ppf = function
  | Invalid_bout_header s ->
      Fmt.pf ppf "invalid BOUT header (%S)" s
  | Unsupported_version v ->
      Fmt.pf ppf "unsupported version (%d > 3)" v

let () =
  Printexc.register_printer begin function
    | ERROR e ->
        Basics.PPrt.string_to Option.some "%a" pp_error e
    | MALFORMED_RAW_TEST ->
        Some "unable to convert malformed raw test into a `.ktest' file"
    | _ ->
        None
  end;;

(* --- *)

module type IO = sig
  type raw_test
  val read
    : [`ktest] file
    -> Sc_values.Struct.typ
    -> options
    -> raw_test Lwt.t
  val write
    : [`ktest] file
    -> Sc_values.Struct.typ
    -> options
    -> raw_test
    -> unit Lwt.t
end

module Make_io (Test_repr: Sc_values.Struct.REPR)
  : IO with type raw_test = Test_repr.Val.t =
struct
  type raw_test = Test_repr.Val.t

  let input_int32 = input_binary_int
  and input_string = really_input_string

  let check_input_header ic = match input_string ic 5 with
    | "KTEST" | "BOUT\n" ->
        ()
    | s ->
        raise @@ ERROR (Invalid_bout_header s)

  let read_header ic =
    check_input_header ic;
    let version = input_int32 ic in
    if version > 3 then
      raise @@ ERROR (Unsupported_version version);
    let nargs = input_int32 ic in
    for _ = 1 to nargs do
      ignore (input_string ic (input_int32 ic))
    done;
    if version >= 2 then begin
      ignore (input_int32 ic);
      ignore (input_int32 ic)
    end

  let label_flag field_name =    (* string of digits are nondet. label inputs *)
    field_name <> "" &&
    match field_name.[0] with '0' .. '9' -> true | _ -> false

  let read_fields: in_channel -> string =
    let buff = Buffer.create 42 in
    let rec aux ic i =
      if i > 0 then
        let field_name = input_string ic (input_int32 ic) in
        let field_val = input_string ic (input_int32 ic) in
        if not (label_flag field_name) then        (* ignore nondet label flags *)
          if field_name = "cstring" then
            Buffer.add_substring buff field_val 0 @@
            String.length field_val - 1               (* ignore terminating 0 *)
          else
            Buffer.add_string buff field_val;
        aux ic (pred i)
    in
    fun ic ->
      Buffer.clear buff;
      aux ic @@ input_int32 ic;
      Buffer.contents buff

  let read f inputs_struct options =
    let< ic = f in
    read_header ic;
    match options.symbolization_scheme with
    | `full_struct ->
        Lwt.return @@ Test_repr.Val.of_string inputs_struct @@ read_fields ic
    | `independent_fields ->
        let packed_struct = read_fields ic in
        let buff = Buffer.create @@ Sc_values.Struct.size inputs_struct in
        let bytes_layout = Sc_values.Struct.bytes_layout inputs_struct in
        let offset =
          List.fold_left begin fun offset (Ctypes_static.BoxedField f, padding) ->
            let f_size = Ctypes.(sizeof @@ field_type f) in
            Buffer.add_substring buff packed_struct offset f_size;
            for _ = 1 to padding; do Buffer.add_char buff '\000'; done;
            offset + f_size
          end 0 bytes_layout
        in
        let encoded_heap =
          String.sub packed_struct offset @@
          String.length packed_struct - offset
        in
        let aligned_struct = Buffer.contents buff ^ encoded_heap in
        Lwt.return @@ Test_repr.Val.of_string inputs_struct aligned_struct

  (* --- *)

  let write_header ~numfields oc =
    let buff = Buffer.create 42 in
    Buffer.add_string buff "BOUT\n";
    Buffer.add_int32_be buff 1l;                                  (* version *)
    Buffer.add_int32_be buff 0l;                                  (* num-args *)
    Buffer.add_int32_be buff (Int32.of_int numfields);
    Buffer.output_buffer oc buff

  let buff =
    Buffer.create 42

  let output_int32 i =                          (* write i in big-endian form *)
    Buffer.add_int32_be buff i

  let output_int i =
    output_int32 (Int32.of_int i)

  let output_string s =
    Buffer.add_string buff s

  let output_char c =
    Buffer.add_char buff c

  let numfields = ref 0

  let output_fieldname fmt =
    Basics.PPrt.string_to begin fun tname ->
      output_int (String.length tname);
      output_string tname;
      incr numfields
    end fmt

  let output_typedfield t str =
    output_fieldname "%a" Sc_values.Printer.c_typ t;
    output_int (String.length str);
    output_string str

  let output_arrayfield t arr_str =
    output_fieldname "%a[_]" Sc_values.Printer.c_typ t;
    output_int (String.length arr_str);
    output_string arr_str

  let output_cstringfield str =
    output_fieldname "cstring";
    output_int (String.length str + 1);
    output_string str;
    output_char '\000'

  let struct_type =
    Sc_values.Struct.as_ctyp ~f:{ f = fun t -> Ctypes_static.BoxedType t }

  let unfold_fields inputs_struct symbolization_scheme raw_repr =
    let string_repr = Test_repr.Val.to_string raw_repr in
    let BoxedType inputs_struct_type = struct_type inputs_struct in
    let offset = ref 0 in
    let read_repr len =
      try
        let res = String.sub string_repr !offset len in
        offset := !offset + len;
        res
      with Invalid_argument _ -> raise MALFORMED_RAW_TEST
    and skip len =
      offset := !offset + len
    in
    let output_root_inputs_struct () =
      match symbolization_scheme with
      | `full_struct ->
          let encoded_struct = read_repr @@ Ctypes.sizeof inputs_struct_type in
          output_typedfield inputs_struct_type encoded_struct
      | `independent_fields ->
          let bytes_layout = Sc_values.Struct.bytes_layout inputs_struct in
          List.iter begin fun (Ctypes_static.BoxedField f, padding) ->
            let f_type = Ctypes.field_type f in
            let f_repr = read_repr @@ Ctypes.sizeof f_type in
            output_typedfield f_type f_repr;
            skip padding
          end bytes_layout
    in
    output_root_inputs_struct ();
    (* Unfold heap-allocated data: *)
    Test_repr.Val.fold_pointers ~deep:true raw_repr () ~f:{
      p = fun t ptr ?len ptr_attrs () ->
        if ptr_attrs.depth > 0 &&    (* not for pointers in root inputs struct *)
           not (Ctypes.is_null ptr)   (* skip null ptr *)
        then match len, ptr_attrs.pv with
          | None, _ | Some 0, _ ->                       (* no subsequent data *)
              ()
          | Some len, `Carray_with_bound_length _
          | Some len, `Carray_with_length_field _ ->
              output_arrayfield t @@ read_repr @@ Ctypes.(sizeof @@ array len t)
          | Some len, `Cstring ->
              output_cstringfield @@ read_repr len
    }

  let write f inputs_struct options raw_repr =
    let> oc = f in
    Buffer.clear buff;
    numfields := 0;
    unfold_fields inputs_struct options.symbolization_scheme raw_repr;
    write_header ~numfields:!numfields oc;
    Buffer.output_buffer oc buff;
    Lwt.return ()

end
