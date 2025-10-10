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
open Fmt

open Lwt.Infix
open Lwt.Syntax

module TYPES = struct

  type ('a, 'p) lwt_process =
    {
      process : 'p;
      promise : 'a Lwt.t
    }
  and 'a process_full = ('a, Lwt_process.process_full) lwt_process
  and 'a process_verbose = ('a, Lwt_process.process) lwt_process
  and 'a process_in = ('a, Lwt_process.process_in) lwt_process
  and 'a process_out = ('a, Lwt_process.process_out) lwt_process
  and 'a process_none = ('a, Lwt_process.process_none) lwt_process

  type command = string array

  (** The type of processes that result in a value of type ['a] *)
  type 'a process =
    | ProcFull of 'a process_full
    | ProcVerbose of 'a process_verbose
    | ProcIn of 'a process_in
    | ProcOut of 'a process_out
    | ProcNone of 'a process_none

  type readable_redirection =
    [ `Grab of stream_grabber
    | `Log of line_logger
    | `GrabNLog of stream_grabber * line_logger
    ]
  and stream_grabber =
    | Stream of (string Lwt_stream.t -> unit Lwt.t)
    | Lines of (string -> unit Lwt.t)
    | MBox of string Lwt_stream.t Lwt_mvar.t
  and line_logger = (string -> unit Lwt.t) Ez_logs.Types.log_lwt

  type command_log_conditions =
    [ `On_exec
    | `On_exit
    | `On_error
    | `Always
    ]

  type redirection =
    [ Lwt_process.redirection
    | readable_redirection
    ]

  type failure = Unix.process_status * command * string

  exception FAILED of failure

end
include TYPES

type 'a t = 'a process

let pp_unix_status fmt stat =
  let str, code, style = match stat with
    | Unix.WEXITED 0 -> "EXITED", 0, `None
    | Unix.WEXITED c ->  "EXITED", c, `Fg `Yellow
    | Unix.WSIGNALED c -> "SIGNALED", c, `Fg `Red
    | Unix.WSTOPPED c -> "STOPPED", c, `Fg `Yellow
  in
  let pp_status = pair ~sep:nop string (Fmt.fmt "(%i)") in
  (styled style @@ styled `Bold pp_status) fmt (str, code)

let pp_cmd_arg fmt arg =
  if String.contains arg ' '
  then Format.fprintf fmt "\"%s\"" arg
  else Format.fprintf fmt "%s" arg

let pp_command =
  (* NB: tates differ w.r.t how commands appear in logs.  I'll use this
     environment variable to customize that.  *)
  let tidy_commands =
    match Env.get ~default:"false" "SC_TIDY_COMMANDS" with
    | "TRUE" | "True" | "true" | "1" -> true
    | "FALSE" | "False" | "false" | "0" -> false
    | v ->
        Log.warn "@[Unknown@ value@ given@ to@ environment@ variable@ \
                  SC_TIDY_COMMANDS:@ %s@]" v;
        false
  in
  let fsep: PPrt.ufmt = if tidy_commands then "@ " else " " in
  styled `Yellow begin fun ppf cmd ->
    let argv = Array.to_list cmd in
    Basics.PPrt.pp_lst pp_cmd_arg ppf argv ~fopen:"@[<2>" ~fsep ~fclose:"@]"
  end

let pp_failure ppf ((pstatus, cmd, cwd) : failure) =
  let str, code = match pstatus with
    | WEXITED c -> "exited", c
    | WSIGNALED c -> "signaled", c
    | WSTOPPED c -> "stopped", c
  in
  Fmt.pf ppf  "Command@ %a@ %s@ with@ code@ %i@ (%s),@ aborting"
    pp_command cmd str code cwd

let () =
  Printexc.register_printer begin function
    | FAILED failure -> Some (Basics.PPrt.to_string "%a" pp_failure failure)
    | _ -> None
  end

let failed_command ?(cwd = Sys.getcwd ()) e cmd =
  raise @@ FAILED (cmd, e, cwd)

let pid: _ t -> int = function
  | ProcFull p -> p.process#pid
  | ProcVerbose p -> p.process#pid
  | ProcIn p -> p.process#pid
  | ProcOut p -> p.process#pid
  | ProcNone p -> p.process#pid

let _lines get p = match get p with
  | None -> Lwt_stream.of_list []
  | Some ic -> Lwt_io.read_lines ic
let _string get p = match get p with
  | None -> Lwt.return ""
  | Some ic -> Lwt_io.read ic

let stdout: 'a t -> Lwt_io.input_channel option = function
  | ProcOut _ | ProcNone _ -> None
  | ProcFull { process; _ }  -> Some process#stdout
  | ProcVerbose { process; _ } -> Some process#stdout
  | ProcIn { process; _ } -> Some process#stdout
let stdout_lines p = _lines stdout p
let stdout_string p = _string stdout p
let stdout_close p = match stdout p with
  | None -> Lwt.return ()
  | Some ic -> Lwt_io.close ic

let stderr: 'a t -> Lwt_io.input_channel option = function
  | ProcVerbose _ | ProcOut _ | ProcIn _ | ProcNone _ -> None
  | ProcFull { process; _ } -> Some process#stderr
let stderr_lines p = _lines stderr p
let stderr_string p = _string stderr p
let stderr_close p = match stderr p with
  | None -> Lwt.return ()
  | Some ic -> Lwt_io.close ic

let stdin: 'a t -> Lwt_io.output_channel option = function
  | ProcIn _ | ProcNone _ -> None
  | ProcFull { process; _ } -> Some process#stdin
  | ProcVerbose { process; _ } -> Some process#stdin
  | ProcOut { process; _ } -> Some process#stdin
let stdin_lines p s = match stdin p with
  | None -> Lwt.return ()
  | Some oc -> Lwt_io.write_lines oc s
let stdin_string p s = match stdin p with
  | None -> Lwt.return ()
  | Some oc -> Lwt_io.write oc s
let stdin_close p = match stdin p with
  | None -> Lwt.return ()
  | Some oc -> Lwt_io.close oc

let join: 'a t -> 'a Lwt.t = fun p ->
  let* res =
    match p with
    | ProcFull p -> p.promise
    | ProcVerbose p -> p.promise
    | ProcIn p -> p.promise
    | ProcOut p -> p.promise
    | ProcNone p -> p.promise
  in
  let* () = stdin_close p <&> stdout_close p <&> stderr_close p in
  Lwt.return res

let get_promise p =
  Lwt.bind p join

(* Handling redirections *)

let nograb: redirection -> Lwt_process.redirection = function
  | #readable_redirection -> `Keep
  | #Lwt_process.redirection as r -> (r :> Lwt_process.redirection)

(** [catch_closed_io f] protects [f] upon reads of a closed channel.  This may
    happen when the channel is an output of a terminated sub-process (under some
    not-yet-elicitd ciscumstances, like if the process has not output any
    line).  *)
let catch_closed_io f () =
  Lwt.catch f begin function
    | Lwt_io.Channel_closed _ -> Lwt.return ()
    | e -> Lwt.reraise e
  end

let grab_stream (grabber: stream_grabber) stream =
  Lwt.async @@ catch_closed_io begin fun () ->
    match grabber with
    | Lines grab_line ->
        Lwt_stream.iter_s grab_line stream
    | Stream grab_stream ->
        grab_stream stream
    | MBox stream_mbox ->
        Lwt_mvar.put stream_mbox stream
  end;
  Lwt.return ()

(** [log_lines log ?header s] passes each line of [s] to log function [log],
    with the given [header] argument. *)
let log_lines (log: line_logger) ?header stream =
  grab_stream (Lines (fun l -> log ?header "%s" l)) stream

module Log_lwt_subproc = (val Ez_logs.anonymous_subproc "_")
let to_subproc_log = `Log Log_lwt_subproc.LWT.debug

let exec ?env ?cwd
    ?(log_command = `Always)
    ?(stdin: Lwt_process.redirection = `Keep)
    ?(stderr = to_subproc_log)
    ?(stdout = to_subproc_log)
    ?timeout
    ?(on_error: (Unix.process_status -> 'a Lwt.t) option)
    ~(on_success: unit -> 'a Lwt.t)
    (cmd: command) : 'a t Lwt.t =
  if log_command = `Always ||
     log_command = `On_exec
  then
    Log.debug "%a%a" pp_command cmd
      Fmt.(if log_command = `On_exec
           then styled `Faint @@ any "@ (with@ quiet@ nominal@ termination)"
           else nop) ();
  let timeout = match timeout with None | Some 0. -> None | t -> t in
  let env = match env with
    | None -> None
    | Some e -> Some (Env.append (Unix.environment ()) e)
  in
  let promise process =
    let* exit_state = process#status in
    if log_command = `Always ||
       log_command = `On_exit ||
       log_command = `On_exec && exit_state <> Unix.WEXITED 0 ||
       log_command = `On_error && exit_state <> Unix.WEXITED 0
    then
      Log.debug "@[<h>%a@ terminated@ with@ status@ %a@]\
                " pp_command cmd pp_unix_status exit_state;
    match exit_state with
    | Unix.WEXITED 0 ->
        on_success ()
    | _ ->
        match on_error with
        | None -> failed_command ?cwd cmd exit_state
        | Some f -> f exit_state
  in
  let cmd = "", cmd in
  let p = match stdin, nograb stderr, nograb stdout with
    | `Keep, `Keep, `Keep ->
        let process
          = Lwt_process.open_process_full cmd ?env ?cwd ?timeout in
        ProcFull { process; promise = promise process }
    | `Keep, stderr, `Keep ->
        let process
          = Lwt_process.open_process cmd ?env ?cwd ?timeout ~stderr in
        ProcVerbose { process; promise = promise process }
    | `Keep, stderr, stdout ->
        let process
          = Lwt_process.open_process_out cmd
            ?env ?cwd ?timeout ~stderr ~stdout in
        ProcOut { process; promise = promise process }
    | stdin, stderr, `Keep ->
        let process
          = Lwt_process.open_process_in cmd
            ?env ?cwd ?timeout ~stdin ~stderr in
        ProcIn { process; promise = promise process }
    | stdin, stderr, stdout ->
        let process
          = Lwt_process.open_process_none cmd
            ?env ?cwd ?timeout ~stdin ~stderr ~stdout in
        ProcNone { process; promise = promise process }
  in
  let async_out_stream s ~log_header ~line_stream =
    match s with
    | `Grab grabber ->
        grab_stream grabber (line_stream p)
    | `Log logger ->
        log_lines logger ~header:log_header (line_stream p)
    | `GrabNLog (grabber, logger) ->
        let s1 = line_stream p in
        let s2 = Lwt_stream.clone s1 in
        Lwt.join [grab_stream grabber s1; log_lines logger s2]
    | #Lwt_process.redirection ->
        Lwt.return ()
  in
  let* () =
    Lwt.join [async_out_stream stdout ~log_header:"." ~line_stream:stdout_lines;
              async_out_stream stderr ~log_header:"*" ~line_stream:stderr_lines]
  in
  Lwt.return p

(** [shell ... cmd] spawns a shell process that executes [cmd]. *)
let shell ?env ?cwd ?log_command ?stdin ?stderr ?stdout ?timeout
    ?(on_error : (Unix.process_status -> 'a Lwt.t) option)
    ~(on_success : unit -> 'a Lwt.t)
    (cmd: string) : 'a t Lwt.t =
  exec ?env ?cwd ?log_command ?stdin ?stderr ?stdout ?timeout
    ?on_error ~on_success (snd @@ Lwt_process.shell cmd)

let shell_unit ?env ?cwd ?log_command ?stdin ?stderr ?stdout ?timeout
    ?(on_error : (Unix.process_status -> 'a Lwt.t) option)
    ?(on_success : unit -> 'a Lwt.t = Lwt.return)
    (cmd: string) : unit Lwt.t =
  get_promise @@
  shell ?env ?cwd ?log_command ?stdin ?stderr ?stdout ?timeout
    ?on_error ~on_success cmd

let shell_status ?(log_command = `On_error) ?(stdout = `Dev_null)
    ?(stderr = `Dev_null) cmd =
  get_promise @@
  shell cmd ~log_command ~stdout ~stderr
    ~on_success: (fun () -> Lwt.return_true)
    ~on_error: (fun _ -> Lwt.return_false)

let exec_status ?(log_command = `On_error) ?(stdout = `Dev_null)
    ?(stderr = `Dev_null) cmd =
  get_promise @@
  exec cmd ~log_command ~stdout ~stderr
    ~on_success: (fun () -> Lwt.return_true)
    ~on_error: (fun _ -> Lwt.return_false)

module PRETTY = struct
  let shell
      ?env ?cwd ?log_command ?stdin ?stderr ?stdout ?timeout
      ?on_error ~on_success fmt =
    Format.kasprintf
      (shell ?env ?cwd ?log_command ?stdin ?stderr ?stdout ?timeout
         ?on_error ~on_success)
      fmt

  let shell_unit
      ?env ?cwd ?log_command ?stdin ?stderr ?stdout ?timeout
      ?on_error ?on_success fmt =
    Format.kasprintf
      (shell_unit
         ?env ?cwd ?log_command ?stdin ?stderr ?stdout ?timeout
         ?on_error ?on_success)
      fmt
end

let pp_proc_type ppf = function
  | ProcFull _ -> Fmt.fmt "fully@ accessible@ sub-process" ppf
  | ProcVerbose _ -> Fmt.fmt "stdin&stdout-only@ sub-process" ppf
  | ProcIn _ -> Fmt.fmt "stdout-only@ sub-process" ppf
  | ProcOut _ -> Fmt.fmt "stdin-only@ sub-process" ppf
  | ProcNone _ -> Fmt.fmt "blind@ sub-process" ppf

let stdout p =
  let res = stdout p in
  if res = None then
    Log.warn "Trying@ to@ read@ from@ stdout@ of@ %a" pp_proc_type p;
  res

let stderr p =
  let res = stderr p in
  if res = None then
    Log.warn "Trying@ to@ read@ from@ stderr@ of@ %a" pp_proc_type p;
  res

let stdin p =
  let res = stdin p in
  if res = None then
    Log.warn "Trying@ to@ write@ to@ stdin@ of@ %a" pp_proc_type p;
  res

let kill ?(delay = 0.) signum p =
  let* () = Lwt_unix.sleep delay in
  match p with
  | ProcFull p -> p.process#kill signum; Lwt.return ()
  | ProcVerbose p -> p.process#kill signum; Lwt.return ()
  | ProcOut p -> p.process#kill signum; Lwt.return ()
  | ProcIn p -> p.process#kill signum; Lwt.return ()
  | ProcNone p -> p.process#kill signum; Lwt.return ()

let terminate ?(delay = 0.) p =
  let* () = Lwt_unix.sleep delay in
  match p with
  | ProcFull p -> p.process#terminate; Lwt.return ()
  | ProcVerbose p -> p.process#terminate; Lwt.return ()
  | ProcOut p -> p.process#terminate; Lwt.return ()
  | ProcIn p -> p.process#terminate; Lwt.return ()
  | ProcNone p -> p.process#terminate; Lwt.return ()
