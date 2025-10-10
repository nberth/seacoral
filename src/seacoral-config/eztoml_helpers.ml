(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Basics

(** Helpers for defining rows in {!Eztoml}.  This is to avoid duplicating type
    definitions.  *)

(** Documentation of each argument:
    - [doc]: documentation of the option (takes in argument the default value of
      the option)

    - [runtime]: sets whether the option only effects the runtime behaviors of
      the tool, in which case it has no impact on the project layout
      (false by default)

    - [check]: additional check on the argument (for example, [fun x -> x > 0]) for
      positive Integers)

    - [check_descr]: documentation for the additional check

    - [default]: a default value for the option
*)
type ('a, 'x) def_with_default_in_doc
  = key: string
  -> ?arg_alias: string list
  -> ?env: string
  -> doc: ('a PPrt.pp -> 'a -> unit) PPrt.fmt
  -> ?runtime:bool
  -> ?check: ('a -> bool)
  -> ?check_descr: PPrt.ufmt
  -> default: 'a
  -> 'x

type ('a, 'x) def_without_default_in_doc
  = key: string
  -> ?arg_alias: string list
  -> ?env: string
  -> doc: PPrt.ufmt
  -> ?runtime:bool
  -> ?check: ('a -> bool)
  -> ?check_descr: PPrt.ufmt
  -> ?default: 'a
  -> 'x

type ('a, 'x) checked_def_with_default_in_doc
  = key: string
  -> ?arg_alias: string list
  -> ?env: string
  -> doc: ('a PPrt.pp -> 'a -> unit) PPrt.fmt
  -> ?runtime:bool
  -> default: 'a
  -> 'x
