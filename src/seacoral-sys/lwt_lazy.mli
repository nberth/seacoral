(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

module Syntax: sig
  val ( &&* ): bool Lwt.t -> bool Lwt.t lazy_t -> bool Lwt.t
  val ( ||* ): bool Lwt.t -> bool Lwt.t lazy_t -> bool Lwt.t
end

(** Type of computations that can be tracked and inhibited by {!do_once} and
    {!ez_do_once}. *)
type ('a, 'b) controlled_computation = ident: 'a -> (unit -> 'b Lwt.t) -> 'b Lwt.t

(** [do_once ~keep_idents (module H)] computes a closure [c] such that [c ~ident
    f] calls [f] once for each value of [ident], and then records and returns
    the result for any subsequent call with [ident].  In case of concurrent
    calls to [c] with identical [ident]s (as per [H.equal]), only one call is
    performed.  If several calls are made with the same identifiers and distinct
    values for [f], one of these functions is arbitrarily chosen and called.

    Typical usage is as follows:
    {v
     let compute_something: arg_type -> res_type Lwt.t =
       let closure: ident:arg_type -> (unit -> res_type Lwt.t) -> res_type Lwt.t =
          Sc_sys.Lwt_lazy.do_once ~keep_idents:true (module struct
            type t = arg_type
            let equal = (=)
            let hash = hashtbl.hash
          end)
       in
       fun arg ->
         closure ~ident:arg begin fun () ->
           ...
         end
    v}

    Note [do_once] is ineffective if called directly without a prior closure
    computation step: in this case [f] is called each time the whole fonction is
    called.

    To avoid unnecessary subsequent calls, [ident] is memoized and recorded in a
    table, along with the result of the call.  If [keep_idents] does not hold
    (the default), a weak hash table is used: this means that subsequent
    re-computations may be expected if identifiers are garbage-collected.  In
    other words, to avoid {b any} recomputation, either [keep_idents] must be
    set, or some other means must be employed to keep references to the
    identifiers around. *)
val do_once
  : ?keep_idents: bool
  -> (module Hashtbl.HashedType with type t = 'k)
  -> ('k, 'b) controlled_computation

(** [ez_do_once ~ident f] is a simplified version of {!do_once}, where
    identifiers are strings, kept internally in a classical (non-week) hash
    table. *)
val ez_do_once
  : (string, 'a) controlled_computation

(** [persist_in ~dir ?force ?donefile doit skip] checks for the existence of a
    file [donefile] in [dir].  It this file does not exist and [force] does not
    hold, [doit ()] is executed, and the file is created empty if [doit]
    succeeds; if the file exists, then [skip] is executed instead and the file
    is left in place.

    Th default value for [donefile] is [".done"], and [force] is [false]. *)
val persist_in
  : dir: File.dir
  -> ?force: bool
  -> ?donefile: string
  -> (unit -> 'a Lwt.t)
  -> (unit -> 'a Lwt.t)
  -> 'a Lwt.t
