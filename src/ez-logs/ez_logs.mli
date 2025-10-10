(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** {2 Logs reporting} *)

(** [reporter ?max_level ?time_precision ppf] returns both a properly setup
    {!Logs} reporter for formatted output on [ppf], and a reference to the level
    below which the reporter does not report log messages.

    The returned referenced level [!max_level] is used to filter out messages:
    if it is [None], nothing is printed; if it is [Some l], then every message
    whose level is above [l] is printed.

    [time_precision] is used to enable and control a timestamp prefix: if it is
    [0] (the default) or negative, then no timesamp is prepended.  Otherwise if
    it is [p], then each message is prepended with the number of seconds elapsed
    since the tool process started, measured with precision [p] and put in
    between square brackets. *)
val reporter
  : ?max_level:Logs.level
  -> ?time_precision:int
  -> Format.formatter
  -> Logs.reporter * Logs.level option ref

(** Combines two reporters so log messages can be broadcast to multiple
    reporters. *)
val combine_reporters: (Logs.reporter as 'r) -> 'r -> 'r

(** [persist_in ~oc ~time_precision reporter] starts persisting logs that are
    output via [reporter] into output stream [oc].  Returns a function that
    should be called to stop reporting on [oc]. *)
val persist_in
  : oc: out_channel
  -> ?time_precision: int
  -> ?style_renderer: Fmt.style_renderer
  -> ?utf_8: bool
  -> ?right_margin: int
  -> Logs.reporter
  -> (unit -> unit)

(** Reference to the log level associated with the reporter on standard output.
    In effect only once {!init_stdout_reporter} has been called. *)
val stdout_level_ref: Logs.level option ref

(** [init_stdout_reporter ~max_level ~time_precision ()] initializes a reporter
    on standard output.  The initial level associated is given via [max_level]
    ([None] by default, meaning that nothing is reported on standard output),
    and can be updated via {!stdout_level_ref} once the call returns. *)
val init_stdout_reporter
  : ?max_level: Logs.level
  -> ?time_precision: int
  -> unit
  -> unit

(* --- *)

module Types : module type of Types

(** Alias for the types of combined log emitter interfaces *)
module type T = Types.T

(** Builds a combined emitter from a given source *)
module From_src (_: Types.SRC) : T

(** Functional version of {!From_src} *)
val from_src: ?styling:Fmt.style list -> Logs.src -> (module T)

(* --- *)

(** [subproc ?format_log_name procname] returns a module suitable for logging
    outputs of sub-processes named [procname].  The name used for log entries is
    formatted using [format_log_name] (default is [Format.asprintf "(%s)"]). *)
val subproc
  : ?format_log_name: (string -> string)
  -> string
  -> (module T)

val anonymous_subproc: string -> (module T)
