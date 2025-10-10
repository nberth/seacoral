(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Sc_sys.File.TYPES

val get_lbls :
  ?filter : (Sc_C.Cov_label.simple -> bool) -> [`labeldb] file ->
  Types.labels

module type S = sig
  val initialize : [> `C | `labelized] file -> unit Lwt.t

  val results: [> `labeldb] Sc_sys.File.t -> Types.lreplay_results Lwt.t
end

module Make (_: Types.CONFIG) : S
