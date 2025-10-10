(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** Environment variables *)

let get ?default key = match Sys.getenv_opt key, default with
  | None, None ->
      Format.kasprintf failwith "Undefined@ environment@ variable@ %s" key
  | None, Some res
  | Some res, _ -> res

let get_opt = Sys.getenv_opt

let append = Array.append            (* for now; check dulpicate definitions? *)

let print fmt e =
  Basics.PPrt.pp_nodelim ~fsep:"@;"
    Format.pp_print_string fmt (Array.to_list e)
