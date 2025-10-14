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

(** Pretty prints coverage statistics. *)
val pp_covinfo: Types.covinfo Fmt.t

val pp_proof_inconsistency: Types.proof_inconsistency Fmt.t
