(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

module TYPES: sig
  type 'a nel =
    | One of 'a
    | (::) of 'a * 'a nel
end
type 'a t = 'a TYPES.nel

val compare_lazy: ('a -> 'a -> int) -> 'a t -> 'a t -> int
val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val hd: 'a t -> 'a
val last: 'a t -> 'a
val fold_left: f:('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val of_list: 'a list -> 'a t
val to_list: 'a t -> 'a list
val cons_list: 'a -> 'a list -> 'a t
val rev_map: ('a -> 'b) -> 'a t -> 'b t
val rev: 'a t -> 'a t
val append: 'a t -> 'a t -> 'a t
val rev_to_list: 'a t -> 'a list
val of_rev_list: 'a list -> 'a t
val map: f:('a -> 'b) -> 'a t -> 'b t
val iter: f:('a -> unit) -> 'a t -> unit
val filter_map: f:('a -> 'b option) -> 'a t -> 'b t option
val exists: f:('a -> bool) -> 'a t -> bool

val pp
  : ?fsep:Basics.PPrt.ufmt
  -> ?fopen:Basics.PPrt.ufmt
  -> ?fclose:Basics.PPrt.ufmt
  -> 'a Fmt.t -> 'a t Fmt.t

module INFIX: sig
  val ( @ ): 'a t -> 'a t -> 'a t
end
