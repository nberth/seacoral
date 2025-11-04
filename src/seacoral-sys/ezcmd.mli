(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Command line manager for external tools. *)

(** The abstract type of command line. *)
module type T = sig
  (** The command. It is created though a accumulation of calls of the following
      primitives. *)
  type t

  (** [make exec]
      Creates a command line for the executable `exec` *)
  val make : string -> t

  (** [raw input cmd]
      Adds `input` right after the current line. *)
  val raw : string -> t -> t

  (** [fmt ppf cmd] appends to [cmd] a raw argument based on the format string
      [ppf] *)
  val rawf: ('a, Format.formatter, unit, t -> t) format4 -> 'a

  (** [base key binding cmd] Adds the binding `key binding` to the command.
      The key is formatted by the `format_key` function (see `Ezcmd.make`) *)
  val base : string -> string -> t -> t

  (** [optional key binding] Same as `base`, but does nothing if either `key` or
      `binding` is None. *)
  val optional : string option -> string option -> t -> t

  (** [if_bool key bool cmd] Adds `key` to the command iff `bool = true`. *)
  val if_bool : string -> bool -> t -> t

  (** [key key cmd] Same as `if_bool key true cmd` *)
  val key: string -> t -> t

  (** [keyf ppf cmd] formats the key according to the format string [ppf] *)
  val keyf: ('a, Format.formatter, unit, t -> t) format4 -> 'a

  (** [must_value key binding cmd] Same as `optional (Some key) binding cmd` *)
  val must_value : string -> string option -> t -> t

  (** [to_cmd cmd] Returns the command as an `Lwt_process.command`. *)
  val to_cmd : t -> string array
end

(** Given a specific key formatting, returns a module for manipulating
    command lines.
    Example:
    `make format_key:(Format.sprintf "-%s")` will return a module in which
    command lines all start with a single dash. This is the module used for
    `frama-c` invocations.
 *)
val make: ?key_value_link:string -> format_key:(string -> string) -> (module T)

(** Functorized version of {!make} *)
module Make: functor
  (_: sig
     val format_key: string -> string
     val key_value_link: string option
   end) -> T

(** Standard command lines: a signe dash for one-character keys and
    two dashes for other keys. *)
module Std : T
