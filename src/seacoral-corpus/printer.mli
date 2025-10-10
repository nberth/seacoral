(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** Pretty-printers for public types of this library. *)

val pp_coverage_info: Types.info Fmt.t
val pp_crash_info: Types.info Fmt.t
val pp_oracle_failures_info: Types.info Fmt.t

val pp_sanitizer_error_summary: Types.sanitizer_error_summary Fmt.t
val pp_test_outcome: Types.test_outcome Fmt.t
