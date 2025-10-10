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

module type S = sig
  val create_annotated_file
    : ?include_dirs:dir list
    -> [> `C] file
    -> Types.label_data Lwt.t

  (** May raise {!Sc_sys.File.MISSING} *)
  val assume_annotated_file
    : [> `C] file
    -> Types.label_data
end

module Make (_: Types.CONFIG) : S

(** Raises {!Types.ERROR} in case Frama-C or a necessary plugin is missing. *)
val check_availability: unit -> unit Lwt.t
