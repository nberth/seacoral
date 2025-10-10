(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

exception Parsing_failure of string

(** {2 Orchestration strategy} *)
type t =
  | Nothing
  | Tool of string
  | Parallel of t NEL.t
  | Sequence of t NEL.t
