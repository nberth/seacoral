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

val lreplay_installed : unit -> bool Lwt.t

module type S = sig
  val lreplay_current_tests
    : covdir: dir
    -> ?extra_testsuite_headers: [`h] file list
    -> [> `C | `labelized] file
    -> unit Lwt.t
end

module Make (_: Types.CONFIG) : S
