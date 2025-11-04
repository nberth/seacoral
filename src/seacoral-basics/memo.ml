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

(** [memoize (module K) ?init_size f] sets up a memoization scheme on [f] by
   using a weak hash table to store its arguments (which must be of type
   [K.t]. *)
let memoize (type k) (module K: Hashtbl.HashedType with type t = k)
    ?(init_size = 11) f =
  let module H = Ephemeron.K1.Make (K) in
  let tbl = H.create init_size in
  fun (x: K.t) -> match H.find_opt tbl x with
    | Some res -> res
    | None -> let res = f x in H.add tbl x res; res

(** Polymorphic memoization that relies on {!Stdlib.compare} and
   {!Hashtbl.hash}.  Note the former has restrictions on the structures it may
   be safely applied to (it may raise {!Invalid_argument} on functional values,
   or not terminate on cyclic structures). *)
let memoize_poly (type k) ?init_size =
  memoize ?init_size
    (module struct
      type t = k
      let equal a b = Stdlib.compare a b = 0
      let hash = Hashtbl.hash
    end)
