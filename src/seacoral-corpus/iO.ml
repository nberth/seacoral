(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025-2026 OCamlPro                                      *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Text I/O primitives *)

open Types

(** {2 RTE "identifiers"} *)

let print_sanitizer_error_summary = function
  | Heap_buffer_overflow addr ->
      Basics.PPrt.asprintf "rte:heap-buffer-overflow\t0x%Lx" addr
  | Invalid_memory_address addr ->
      Basics.PPrt.asprintf "rte:invalid-memory-address\t0x%Lx" addr
  | Arithmetic_error addr ->
      Basics.PPrt.asprintf "rte:arithmetic-error\t0x%Lx" addr

let print_cover_summary ints =
  Basics.PPrt.asprintf
    "cover\t%a"
    (fun fmt l -> List.iter (fun i -> Fmt.int fmt i; Fmt.string fmt "\t") l)
    (Basics.Ints.elements ints) 

let print_oracle_fail () = "oracle-fail\t"

let print_summary () = function
  | Triggering_RTE err -> print_sanitizer_error_summary err
  | Covering_label i -> print_cover_summary i
  | Oracle_failure -> print_oracle_fail ()

let scan_list ic =
  let rec loop acc =
    try
      Scanf.bscanf ic "%i\t" begin fun i ->
        loop (Basics.Ints.add i acc)
        end
    with
    | End_of_file -> acc
  in
  loop Basics.Ints.empty

let scan_summary (ic: Scanf.Scanning.in_channel) =
  Scanf.bscanf ic "%s@\t" begin function
    | "rte:heap-buffer-overflow" ->
        Scanf.bscanf ic "0x%Lx"
          (fun addr -> Triggering_RTE (Heap_buffer_overflow addr))
    | "rte:invalid-memory-address" ->
        Scanf.bscanf ic "0x%Lx"
          (fun addr -> Triggering_RTE (Invalid_memory_address addr))
    | "rte:arithmetic-error" ->
        Scanf.bscanf ic "0x%Lx"
          (fun addr -> Triggering_RTE (Arithmetic_error addr))
    | "cover" -> Covering_label (scan_list ic)
    | "oracle-fail" -> Oracle_failure
    | key ->
        raise (Scanf.Scan_failure (Fmt.str "unknown sanitizer error key %S" key))
  end
