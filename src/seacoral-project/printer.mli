(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

val pp_entrypoint_name: _ Types.project Fmt.t

val pp_coverage_info: Types.info Fmt.t
val pp_crash_info: Types.info Fmt.t
val pp_oracle_failures_info: Types.info Fmt.t

val pp_setup_error: Types.setup_error Fmt.t
val pp_labeling_error: Types.labeling_error Fmt.t
val pp_elaboration_error: Types.elaboration_error Fmt.t

module C: sig
  val emit_testcall
    : oracle_assessment: string
    -> emit_effective_inputs: 'a Fmt.t
    -> _ Types.project_params
    -> 'a
    -> Format.formatter -> unit
end

val pp_raw_test
  : project: 'raw_test Types.project
  -> 'raw_test Fmt.t
val pp_test_view
  : ?sep: Basics.PPrt.ufmt
  -> project: 'raw_test Types.project
  -> 'raw_test Sc_corpus.Types.test_view Fmt.t
val pp_tests
  : project: 'raw_test Types.project
  -> 'raw_test Sc_corpus.Types.test_view list Fmt.t
