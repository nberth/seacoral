(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** {2 Logs reporting} *)

let ref_time = Unix.gettimeofday ()

let level_compare: Logs.level -> Logs.level -> int = fun l l' ->
  if l = l' then 0 else match l, l' with
    | App,   _ -> -1   | _, App   -> 1
    | Error, _ -> -1   | _, Error -> 1
    | Warning, _ -> -1 | _, Warning -> 1
    | Info, _ -> -1    | _, Info -> 1
    | Debug, _ -> -1 (*| _, Debug -> 1*)

let has_level ?max_level l = match max_level with
  | Some max_level -> level_compare l max_level <= 0
  | None -> false

let reporter max_level_ref ?(time_precision = 0) ppf =
  let open Logs in
  let open Logs_fmt in
  let open Format in
  let level_char = function
    | App -> 'A'
    | Error -> 'E'
    | Warning -> 'W'
    | Info -> 'I'
    | Debug -> 'D'
  and level_style = function
    | App -> app_style
    | Error -> err_style
    | Warning -> warn_style
    | Info -> info_style
    | Debug -> debug_style
  and has_level l =
    has_level ?max_level:!max_level_ref l
  in
  let pp_level fmt l =
    fprintf fmt "[%c]" (level_char l)
  in
  let pp_header fmt (l, tags) =
    if time_precision > 0 then
      Fmt.styled (`Fg `Magenta)
        (fun fmt -> Fmt.fmt "[%.*f]" fmt time_precision)
        fmt (Unix.gettimeofday () -. ref_time);
    Fmt.styled (level_style l) begin fun fmt -> function
      | None -> pp_level fmt l
      | Some s -> fprintf fmt "%a[%s]" pp_level l s
    end fmt tags;
  and pp_src =
    Fmt.styled `Faint @@
    Fmt.styled (`Fg `Cyan) @@
    (fun fmt src -> pp_print_string fmt (Logs.Src.name src))
  in
  let report src level ~over k msgf =
    let with_stamp h _tags k ppf fmt =
      (if has_level level then kfprintf else ikfprintf)
        k ppf ("%a{%a} @[" ^^ fmt ^^ "@]@.")
        pp_header (level, h)
        pp_src src
    in
    msgf @@ fun ?header ?tags fmt ->
    with_stamp header tags (fun _fmt -> over (); k ()) ppf fmt
  in
  { report }

let stdout_level_ref = ref None

(** Always enable debug level for potential log files (see below); filtering for
    the console happends at the reporter level, by setting
    {!stdout_level_ref}. *)
let init_stdout_reporter ?max_level ?time_precision () =
  stdout_level_ref := max_level;
  Logs.set_reporter @@ reporter stdout_level_ref ?time_precision Fmt.stdout

let reporter ?max_level ?time_precision ppf =
  let max_level_ref = ref max_level in
  reporter max_level_ref ?time_precision ppf, max_level_ref

let combine_reporters Logs.{ report = r1 } Logs.{ report = r2 } =
  Logs.{
    report = fun src level ~over k msgf ->
      r2 src level ~over (fun () -> r1 src level ~over:Fun.id k msgf) msgf
  }

let persist_in ~oc ?(time_precision = 4) ?style_renderer ?utf_8
    ?(right_margin = max_int) base_reporter =
  let log_fmt = Format.formatter_of_out_channel oc in
  Format.pp_set_margin log_fmt right_margin;
  Option.iter (Fmt.set_style_renderer log_fmt) style_renderer;
  Option.iter (Fmt.set_utf_8 log_fmt) utf_8;
  let close_log () =
    Logs.set_reporter base_reporter;
    Fmt.flush log_fmt ();
    close_out oc;
  in
  let log_rprt, _ = reporter log_fmt ~max_level:Logs.Debug ~time_precision in
  Logs.set_reporter (combine_reporters log_rprt base_reporter);
  close_log

(* --- *)

module Types = Types
module type T = Types.T
module From_src (S: Types.SRC) = struct
  include S
  module K = (val (Logs.src_log src))
  let apply_styling ppf (pp: Format.formatter -> unit) : unit =
    let rec aux ppf = function
      | [] -> pp ppf
      | style :: tl -> Fmt.styled style aux ppf tl
    in
    aux ppf styling
  let msg ~level (log: _ Logs.log) ?header fmt =
    if has_level ?max_level:(Logs.Src.level src) level
    then Format.kdprintf (fun pp -> log (fun p -> p ?header "%a" apply_styling pp)) fmt
    else Format.ifprintf Fmt.stderr fmt
  let app   ?header fmt = msg ~level:Logs.App     K.app   ?header fmt
  let err   ?header fmt = msg ~level:Logs.Error   K.err   ?header fmt
  let warn  ?header fmt = msg ~level:Logs.Warning K.warn  ?header fmt
  let info  ?header fmt = msg ~level:Logs.Info    K.info  ?header fmt
  let debug ?header fmt = msg ~level:Logs.Debug   K.debug ?header fmt
  module K_lwt = (val (Logs_lwt.src_log src))
  module LWT = struct
    let msg ~level (log: _ Logs_lwt.log) ?header fmt =
      if has_level ?max_level:(Logs.Src.level src) level
      then Format.kdprintf (fun pp -> log (fun p -> p ?header "%a" apply_styling pp)) fmt
      else Format.ikfprintf (fun _ -> Lwt.return ()) Fmt.stderr fmt
    let app   ?header fmt = msg ~level:Logs.App     K_lwt.app   ?header fmt
    let err   ?header fmt = msg ~level:Logs.Error   K_lwt.err   ?header fmt
    let warn  ?header fmt = msg ~level:Logs.Warning K_lwt.warn  ?header fmt
    let info  ?header fmt = msg ~level:Logs.Info    K_lwt.info  ?header fmt
    let debug ?header fmt = msg ~level:Logs.Debug   K_lwt.debug ?header fmt
    let err_if_rejected ?(silence = fun _ -> false) f =
      Lwt.catch f begin fun exn ->
        if silence exn then Lwt.reraise exn else
          let backtrace = String.trim @@ Printexc.get_backtrace () in
          let error_msg = Printexc.to_string exn in
          let red pp = Fmt.styled (`Fg `Red) pp in
          let bold pp = Fmt.styled `Bold pp in
          Lwt.bind begin
            err "%a@\n%a"
              Fmt.(red @@ bold @@ hbox text) error_msg
              Fmt.(red @@ vbox @@ list ~sep:sp string)
              (String.split_on_char '\n' backtrace)
          end begin fun () ->
            Lwt.reraise exn
          end
      end
  end
end
let from_src ?(styling = []) src =
  (module From_src (struct let src = src let styling = styling end): Types.T)

(* --- *)

let subproc ?(format_log_name = Format.asprintf "(%s)") procname =
  from_src (Logs.Src.create (format_log_name procname))
    ~styling:[`Faint]

let anonymous_subproc srcname =
  subproc ~format_log_name:(Format.asprintf "%s") srcname
