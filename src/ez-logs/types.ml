(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Module signatures for unified logging interfaces. *)

(** Module argument, for {!Ez_logs.From_src}. *)
module type SRC = sig
  val src: Logs.src
  val styling: Fmt.style list
end

type 'a log = ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
type 'a log_lwt =
  ?header:string -> ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

(** Unified interface for emitting log messages: gathers direct pretty-printing
    functions, along with the original lazy/continuation-style functions of the
    {!Logs} library.  Each case comes with or without {!Lwt} support. *)
module type T = sig

  include SRC

  (** Original continuation-style interfaces *)

  module K: Logs.LOG
  module K_lwt: Logs_lwt.LOG


  (** New, direct pretty-printing interfaces *)

  val app: 'a log
  val err: 'a log
  val warn: 'a log
  val info: 'a log
  val debug: 'a log
  module LWT: sig
    val app: 'a log_lwt
    val err: 'a log_lwt
    val warn: 'a log_lwt
    val info: 'a log_lwt
    val debug: 'a log_lwt
    val err_if_rejected: ?silence:(exn -> bool) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  end
end
