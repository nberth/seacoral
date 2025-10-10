(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** {2 Collections over C types} *)

module KEY: sig
  include Basics.ORDERED_TYPE
  val of_typ: _ Ctypes.typ -> t
  val to_boxed_typ: t -> Ctypes_static.boxed_typ

  (** [to_string key] always returns a valid C symbol *)
  val to_string: t -> string
end
module SET: Basics.SET with type elt = KEY.t
module MAP: Basics.MAP with type key = KEY.t

(** Aliases that expose shorter yet readable names for the modules above. *)
module ALIASES: sig
  module Ctype_key = KEY
  module Ctype_set = SET
  module Ctype_map = MAP
end
