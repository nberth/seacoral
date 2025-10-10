(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

module type Crunched = sig
  val file_list: string list
  val read: string -> string option
  val hash: string -> string option
  val size: string -> int option
end

val deflate_in: dir:Sc_sys.File.dir -> (module Crunched) -> unit

module LWT: sig
  val deflate_in: dir:Sc_sys.File.dir -> (module Crunched) -> unit Lwt.t
  val deflate_once_in: dir:Sc_sys.File.dir -> ident:string -> (module Crunched) ->
    unit Lwt.t
end
