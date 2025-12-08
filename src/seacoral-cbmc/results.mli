(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

type test_type =
  | Labels of Basics.Ints.t
  | RTE of Types.DATA.assertion_check

type coverable = [ `Test of Sc_values.literal_binding * test_type ]

type res = [
    coverable
  | `Uncov of int
  | `Extra of Types.DATA.assertion_check
  ]

type t

val empty : t

(** Returns the tests registered. *)
val get_tests :
  t -> (Sc_values.literal_binding * test_type) list

(** Returns the covered labels. *)
val get_covered : t -> Basics.Ints.t

(** Returns the uncoverable labels. If there are non valid extra properties,
    returns the empty set. *)
val get_uncoverable : t -> Basics.Ints.t

val goal_stream_to_test_cases_stream :
  env:Types.simple_label_env
  -> harness:Harness.t
  -> stream:Types.DATA.cbmc_cover_output Types.DATA.cell Lwt_stream.t
  -> res Lwt_stream.t

(** Same as [goal_stream_to_test_cases_stream] for a CBMC
    assert/clabel analysis. *)
val assert_data_stream_to_test_cases_stream :
  env:Types.simple_label_env ->
  harness:Harness.t ->
  stream:Types.DATA.cbmc_assert_output Types.DATA.cell Lwt_stream.t ->
  res Lwt_stream.t 

(** Returns the data content of a list. *)
val only_data : 'a Types.DATA.cell list -> 'a list

val summing_up : res list -> t
