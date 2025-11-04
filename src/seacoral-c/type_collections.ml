(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** {2 Collections over C types} *)

open Basics

module KEY = struct
  type t = tag * Ctypes_static.boxed_typ
  and tag =
    | Tag of string                                        (* may have spaces *)
    | Ptr of (t * int)
  let rec compare (a, _) (b, _) = match a, b with
    | Tag a, Tag b -> String.compare a b
    | Ptr a, Ptr b -> cmp_pair compare Integer.compare a b
    | Tag _, _ -> -1
    | _, Tag _ -> 1
  and cmp_pair cmp cmp' (a1, b1) (a2, b2) =
    let r = cmp a1 a2 in if r != 0 then r else cmp' b1 b2
  let rec print ppf = function
    | Tag a, _ -> Fmt.string ppf a
    | Ptr (a, len), _ -> Fmt.pf ppf "%dptr_" len; print ppf a
  let to_string t =
    String.replace_spaces ~by:'_' @@
    PPrt.to_string "typ_%a" print t
  let ptr ?(declared_length = 1) t =
    Ptr (t, declared_length)
  let rec of_typ: type k. k Ctypes.typ -> t = fun t ->
    let tag = match t with
      | Void
      | Primitive _
      | View _ ->
          Tag (Ctypes.string_of_typ t)
      | Struct { tag = n; _ } | Union { utag = n; _ } ->
          Tag n
      | Pointer t ->
          ptr (of_typ t)
      | Array (t, declared_length) ->
          ptr (of_typ t) ~declared_length
      | t ->
          raise @@ Types.UNSUPPORTED_TYPE (Ctypes_static.BoxedType t)
    in
    tag, Ctypes_static.BoxedType t
  let to_boxed_typ: t -> Ctypes_static.boxed_typ = snd
end
module SET = MakeSet (KEY)
module MAP = MakeMap (KEY)

module ALIASES = struct
  module Ctype_key = KEY
  module Ctype_set = SET
  module Ctype_map = MAP
end
