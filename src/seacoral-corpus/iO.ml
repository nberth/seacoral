(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Text I/O primitives *)

open Types

(** {2 RTE "identifiers"} *)

let print_sanitizer_error_summary () = function
  | Heap_buffer_overflow addr ->
      Basics.PPrt.asprintf "heap-buffer-overflow\t0x%Lx" addr
  | Invalid_memory_address addr ->
      Basics.PPrt.asprintf "invalid-memory-address\t0x%Lx" addr
  | Arithmetic_error addr ->
      Basics.PPrt.asprintf "arithmetic-error\t0x%Lx" addr

let scan_sanitizer_error_summary (ic: Scanf.Scanning.in_channel) =
  Scanf.bscanf ic "%s@\t" begin function
    | "heap-buffer-overflow" ->
        Scanf.bscanf ic "0x%Lx" (fun addr -> Heap_buffer_overflow addr)
    | "invalid-memory-address" ->
        Scanf.bscanf ic "0x%Lx" (fun addr -> Invalid_memory_address addr)
    | "arithmetic-error" ->
        Scanf.bscanf ic "0x%Lx" (fun addr -> Arithmetic_error addr)
    | key ->
        raise (Scanf.Scan_failure (Fmt.str "unknown sanitizer error key %S" key))
  end
