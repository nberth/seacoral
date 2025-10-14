(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** Abstract representation of CBMC results. *)

type t

(** Returns the tests registered. *)
val get_tests :
  t -> (Sc_values.literal_binding * Basics.Ints.t) list

(** Returns the covered labels. *)
val get_covered : t -> Basics.Ints.t

(** Returns the uncoverable labels. If there are non valid extra properties,
    returns the empty set. *)
val get_uncoverable : t -> Basics.Ints.t

(** Reads the result of a CBMC cover analysis. *)
val goals_to_test_cases :
  env:Types.simple_label_env ->
  harness:Harness.t ->
  Types.DATA.cbmc_cover_output Types.DATA.cell list ->
  t

(** Reans the result of a CBMC assert/clabel analysis. *)
val assert_data_list_to_test_cases :
  env:Types.simple_label_env ->
  harness:Harness.t ->
  Types.DATA.assertion_check list Types.DATA.cell list ->
  t

(** Returns the data content of a list. *)
val only_data : 'a Types.DATA.cell list -> 'a list
