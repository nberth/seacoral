(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

module type Key = sig
  val format_key : string -> string
  val key_value_link : string option
end

module type T = sig
  type t

  val make : string -> t

  val raw : string -> t -> t

  val rawf: ('a, Format.formatter, unit, t -> t) format4 -> 'a

  val base : string -> string -> t -> t

  val optional : string option -> string option -> t -> t

  val if_bool : string -> bool -> t -> t

  val key: string -> t -> t

  val keyf: ('a, Format.formatter, unit, t -> t) format4 -> 'a

  val must_value : string -> string option -> t -> t

  val to_cmd : t -> string array
end

module Make(K : Key) : T = struct
  type t = string list                 (* reversed list of command-line items *)

  let make str = [str]

  let raw = List.cons

  let rawf ppf = Format.kasprintf raw ppf

  let builder ?(no_value_means_no_option = false) key value t =
    match key, value, K.key_value_link with
    | None, _, _ -> t
    | Some _, None, _ when no_value_means_no_option -> t
    | Some k, None, _ -> K.format_key k :: t
    | Some k, Some v, None -> v :: K.format_key k :: t
    | Some k, Some v, Some sep -> rawf "%s%s%s" (K.format_key k) sep v t

  let optional = builder ~no_value_means_no_option:true

  let if_bool (key : string) (flag : bool) (t : t) : t =
    if flag
    then builder (Some key) None t
    else t

  let key k = if_bool k true

  let keyf ppf = Format.kasprintf key ppf

  let must_value (key : string) (value : string option) : t -> t =
    builder ~no_value_means_no_option:true  (Some key) value

  let base (key : string) (value : string) : t -> t =
    builder (Some key) (Some value)

  let to_cmd (t : t) =
    Array.of_list (List.rev t)
end

(* warning 16: unerasable-optional-argument *)
let [@warning "-16"] make ?key_value_link ~format_key : (module T) =
  (module Make (struct
       let format_key = format_key
       let key_value_link = key_value_link
     end))

module Std = Make (struct
    let format_key s =
      if String.length s = 1
      then Format.sprintf "-%s" s
      else Format.sprintf "--%s" s
    let key_value_link = None
  end)
