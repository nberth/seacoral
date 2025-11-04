(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Sc_sys.File.TYPES

(** Checks if e-acsl is installed *)
val is_installed
  : ?eacsl_gcc_path: string
  -> unit
  -> bool Lwt.t

module type S = sig

  (** Calls e-acsl on a test and returns true if the acsl specifications are
      satisfied by the test. *)
  val call_eacsl_on_file
    : ?eacsl_gcc_path: string
    -> codebase:[`C] file list
    -> [`C] file
    -> bool Lwt.t

end

module Make (_: Types.CONFIG) : S
