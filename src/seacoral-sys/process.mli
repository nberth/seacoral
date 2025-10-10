(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** Safe process manager. Allows to start shell processes and monitor them. *)

module TYPES: sig

  (** The type of processes that result in a value of type ['a] *)
  type 'a process

  type command = string array

  (** Default type for errors raised when a spawned process errors out. *)
  type failure = private Unix.process_status * command * string

  exception FAILED of failure

  (** Possible redirections for standard output and errors of processes spawned
      using {!exec} and {!shell} below.  In addition to
      {!Lwt_process.redirection}, [`GrabLines f] allows to pass the stream of
      lines to a function [f], and [`Log logfun] passes each individual line to
      [logfun]. *)
  type redirection =
    [ Lwt_process.redirection
    | `Grab of stream_grabber
    | `Log of line_logger
    | `GrabNLog of stream_grabber * line_logger
    ]

  (** Grabber for output streams (stdout or stderr).  One may grab the stream as
      a whole using a promise or a mailbox variable, or its lines individially
      in sequence.

      Note it is an error to assume that promises given via [Stream] or [Lines]
      are always executed before the sub-process terminates and its status is
      retrieved.  Using [Mbox] is required to ensure the stream of lines is
      retrieved {e before} {!FAILED} is caught or any one of user-given
      [on_success] or [on_error] functions is called.

      To use [MBox], first instantiate an {e empty} mailbox variable, and then
      pass it to the process spawning function (if the variable is not empty
      this function may hang and never terminate).  The variable is then
      assigned with the stream {e before} the spawning function terminates.  A
      correct pattern to achieve this (to, for instance, grab the standard error
      in case the sub-process errors out), is as follows:

      {v
        let stderr_lines_mbox = Lwt_mvar.create_empty () in
        Lwt.catch begin fun () ->
          Sc_sys.Process.exec ...
            ~stderr:(`Grab (MBox stderr_lines_mbox))
        end begin function
          | Sc_sys.Process.FAILED _ ->
            let* stderr_lines = Lwt_mvar.take stderr_lines_mbox in
            ...
          | e ->
            Lwt.reraise e
        end
      v} *)
  and stream_grabber =
    | Stream of (string Lwt_stream.t -> unit Lwt.t)
    | Lines of (string -> unit Lwt.t)
    | MBox of string Lwt_stream.t Lwt_mvar.t

  (** Logger for individial lines. *)
  and line_logger = (string -> unit Lwt.t) Ez_logs.Types.log_lwt

  type command_log_conditions =
    [ `On_exec
    | `On_exit
    | `On_error
    | `Always
    ]

end
include module type of TYPES
  with type 'a process = 'a TYPES.process
   and type stream_grabber = TYPES.stream_grabber
   and type redirection = TYPES.redirection

type 'a t = 'a process

(* --- *)

(** [exec ?env ?cwd ?stdin ?stderr ?stdout ?timeout ?on_error ~on_success
    command] spawns a new process using [command].  Standard input, output, and
    error streams are handled according to their respective optional arguments,
    where default values for [stderr] and [stdout] correspond to logging of each
    line using the log module attached to {!Process}.

    The optional [timeout] indicates a delay in seconds after which the process
    is sent a {!Unix.sigkill} signal (and I/O channels are closed). *)
val exec
  : ?env:string array
  -> ?cwd:string
  -> ?log_command:command_log_conditions
  -> ?stdin:Lwt_process.redirection
  -> ?stderr:redirection
  -> ?stdout:redirection
  -> ?timeout: float
  -> ?on_error:(Unix.process_status -> 'a Lwt.t)
  -> on_success:(unit -> 'a Lwt.t)
  -> command
  -> 'a t Lwt.t

(** Similar to {!exec}, except [command] is launched in a shell. *)
val shell
  : ?env:string array
  -> ?cwd:string
  -> ?log_command:command_log_conditions
  -> ?stdin:Lwt_process.redirection
  -> ?stderr:redirection
  -> ?stdout:redirection
  -> ?timeout:float
  -> ?on_error:(Unix.process_status -> 'a Lwt.t)
  -> on_success:(unit -> 'a Lwt.t)
  -> string
  -> 'a t Lwt.t

(** Spawns a {!shell} command that promises a unit. *)
val shell_unit
  : ?env:string array
  -> ?cwd:string
  -> ?log_command:command_log_conditions
  -> ?stdin:Lwt_process.redirection
  -> ?stderr:redirection
  -> ?stdout:redirection
  -> ?timeout:float
  -> ?on_error:(Unix.process_status -> unit Lwt.t)
  -> ?on_success:(unit -> unit Lwt.t)
  -> string
  -> unit Lwt.t

(** Returns with the process status whenever it terminates *)
val exec_status
  : ?log_command:command_log_conditions
  -> ?stdout:redirection
  -> ?stderr:redirection
  -> command
  -> bool Lwt.t

(** Same as `exec_status` but with a shell command *)
val shell_status
  : ?log_command:command_log_conditions
  -> ?stdout:redirection
  -> ?stderr:redirection
  -> string
  -> bool Lwt.t

module PRETTY: sig
  val shell
    : ?env:string array
    -> ?cwd:string
    -> ?log_command:command_log_conditions
    -> ?stdin:Lwt_process.redirection
    -> ?stderr:redirection
    -> ?stdout:redirection
    -> ?timeout:float
    -> ?on_error:(Unix.process_status -> 'a Lwt.t)
    -> on_success:(unit -> 'a Lwt.t)
    -> (_, 'a t Lwt.t) Basics.PPrt.func
  val shell_unit
    : ?env:string array
    -> ?cwd:string
    -> ?log_command:command_log_conditions
    -> ?stdin:Lwt_process.redirection
    -> ?stderr:redirection
    -> ?stdout:redirection
    -> ?timeout:float
    -> ?on_error:(Unix.process_status -> unit Lwt.t)
    -> ?on_success:(unit -> unit Lwt.t)
    -> (_, unit Lwt.t) Basics.PPrt.func
end

(** Returns the promise associated with the process *)
val join: 'a t -> 'a Lwt.t
val get_promise: 'a t Lwt.t -> 'a Lwt.t                       (* temporary alias *)

val pid: _ t -> int

(** Returns the standard output of the process *)
val stdout: 'a t -> Lwt_io.input_channel option
val stdout_lines: 'a t -> string Lwt_stream.t
val stdout_string: 'a t -> string Lwt.t
val stdout_close: 'a t -> unit Lwt.t

(** Returns the standard error of the process *)
val stderr: 'a t -> Lwt_io.input_channel option
val stderr_lines: 'a t -> string Lwt_stream.t
val stderr_string: 'a t -> string Lwt.t
val stderr_close: 'a t -> unit Lwt.t

(** Returns the standard input of the process *)
val stdin: 'a t -> Lwt_io.output_channel option
val stdin_lines: 'a t -> string Lwt_stream.t -> unit Lwt.t
val stdin_string: 'a t -> string -> unit Lwt.t
val stdin_close: 'a t -> unit Lwt.t

(** [pp_unix_status fmt status]
    Prints the unix process status *)
val pp_unix_status : Format.formatter -> Unix.process_status -> unit

(** Prints an lwt_process command *)
val pp_command : Format.formatter -> string array -> unit

val pp_failure: failure Fmt.t

(** [kill ?delay signum proc] waits for [delay] seconds if this argument is
   provided, and then sends the signal [signum] to the process if it is still
   running. *)
val kill: ?delay:float -> int -> 'a t -> unit Lwt.t

(** [terminate ?delay proc] waits for [delay] seconds if this argument is
    provided, and then terminates the process if it is still running. *)
val terminate: ?delay:float -> 'a t -> unit Lwt.t
