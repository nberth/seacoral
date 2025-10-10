(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Infix
open Lwt.Syntax
open File.Syntax

(** {2 Inotify-related utilities} *)

let watchdir ~inot dir evts =
  (* Ensure we're watching a directory *)
  try
    Lwt_inotify.add_watch inot (File.absname dir) (S_Onlydir :: evts)
  with Unix.Unix_error _ ->
    Lwt.fail @@ File.missing dir

let rec waitfor: 'k. ?inot:_ -> 'k File.t -> _ = fun ?inot file ->
  let open Inotify in
  if File.is_dir file
  then Lwt.return ()
  else begin
    let parent, filename = File.parent file, File.basename file in
    let* inot = match inot with
      | Some inot -> Lwt.return inot
      | None -> Lwt_inotify.create ()
    in
    let* () = waitfor ~inot parent in
    let* wtch = watchdir ~inot parent [S_Create] in
    let rec wait () =
      let* evt = Lwt_inotify.read inot in
      match evt with
      | w, _, _, _ when w <> wtch ->
          wait ()
      | _, ek, _, Some f when f = filename && List.mem Isdir ek ->
          Lwt.return ()
      | _ ->
          wait ()
    in
    let* () = wait () in
    Lwt_inotify.rm_watch inot wtch
  end

let monitor_entry ~on_event entry =
  let open Inotify in
  let pp_event fmt e = Format.pp_print_string fmt (string_of_event e) in
  let* inot = Lwt_inotify.create () in
  (* Checks `dir` is indeed a directory. *)
  let* wtch = match entry with
    | `Dir d ->
        (* Wait for watched directory to exist, in case. *)
        let* () = waitfor ~inot d in
        watchdir ~inot d [S_Close_write]
    | `File f ->
        (* Wait for directory including f to exist, in case. *)
        let* () = waitfor ~inot (File.parent f) in
        Lwt_inotify.add_watch inot (File.absname f) [S_Close_write]
  in
  (* Synchronization variable used to wait for `inot`'s underlying queue to be
     empty before calling `Lwt_inotify.close inot` and terminating
     `stop_watch`. *)
  let stopped: unit Lwt_mvar.t = Lwt_mvar.create_empty () in
  let rec watch () =
    let* evt = Lwt_inotify.read inot in
    match evt, entry with
    | (w, _, _, _), _ when w <> wtch ->
        watch ()
    | (_, ek, _, Some f), `Dir d when List.mem Close_write ek ->
        on_event @@ d / f >>= watch
    | (_, ek, _, _), `File f when List.mem Close_write ek ->
        on_event f >>= watch
    | (_, ek, _, _), _ when List.mem Ignored ek ->
        Lwt_mvar.put stopped ()
    | _ ->
        Log.warn "Unexpected inotify event: %a" pp_event evt;
        watch ()
  and stop_watch () =
    let* () = Lwt_inotify.rm_watch inot wtch in
    let* () = Lwt_mvar.take stopped in         (* wait for `inot` to be empty *)
    Lwt_inotify.close inot
  in
  Lwt.async watch;
  Lwt.return stop_watch

let monitor_dir ~on_close dir =
  monitor_entry ~on_event:on_close (`Dir dir)

let monitor_file ~on_close file =
  monitor_entry ~on_event:on_close (`File file)

(* --- *)

(** Asynchronous versions *)
module ASYNC = struct

  let monitoring_error ppf (f, exn) =
    Fmt.pf ppf "Unable@ to@ handle@ `%a':@ @[%a@]"
      File.print f Fmt.exn_backtrace (exn, Printexc.get_raw_backtrace ())

  let monitor_dir ~on_close indir =
    let on_close, await_all =
      Lwt_task.async_start_sync_stop begin fun f ->
        Lwt.catch begin fun () ->
          on_close f
        end begin fun e ->
          Log.LWT.err "%a" (Fmt.styled `Red monitoring_error) (f, e);
        end
      end
    in
    let* stop_monitoring = monitor_dir indir ~on_close in
    Lwt.return (fun () -> stop_monitoring () >>= await_all)

end
