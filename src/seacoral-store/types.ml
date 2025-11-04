(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

type params =
  {
    num_labels: int;
    inhibit_auto_termination: bool;
  }

(** Coverage information. Each label is identified by a unique integer. *)
type covinfo =
  {
    covered_ids: Basics.Ints.t;     (** set of covered labels. *)
    uncoverable_ids: Basics.Ints.t; (** set of uncoverable labels. *)
    num_ids: int;                   (** total number of labels. *)
  }

(** {2 Exceptions} *)

type proof_inconsistency =
  {
    old_status: (string * [`Cov | `Uncov | `Unk]) Basics.IntMap.t;
    new_status: [`Cov | `Uncov | `Unk];
    toolname: string;
  }

(** Raised by {!Main.share_status} and other tools when inconsistent results are
    obtained for a given set of proof objectives. *)
exception Proof_inconsistency of proof_inconsistency
