(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** {2 Memoization} *)

(** [memoize (module K) ?init_size f]

    Sets up a memoization scheme on [f] by using a weak hash table to store
    its arguments (which must be of type [K.t]). *)
val memoize :
  (module Hashtbl.HashedType with type t = 'k) ->
  ?init_size:int -> ('k -> 'a) -> 'k -> 'a

(** Polymorphic memoization that relies on {!Stdlib.compare} and
   {!Hashtbl.hash}.  Note the former has restrictions on the structures it may
   be safely applied to (it may raise {!Invalid_argument} on functional values,
   or not terminate on cyclic structures). *)
val memoize_poly : ?init_size:int -> ('k -> 'a) -> 'k -> 'a
