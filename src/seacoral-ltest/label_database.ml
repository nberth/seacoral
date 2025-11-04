(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Types
open Basics

open Sc_sys.File.Syntax
open Lwt.Syntax

let log_src =
  Logs.Src.create ~doc:"Logs of Label Database" "Sc_ltest.Label_database"

module Log = (val (Ez_logs.from_src log_src))



module type S = sig
  val initialize : [> `C | `labelized] Sc_sys.File.t -> unit Lwt.t

  val results: [> `labeldb] Sc_sys.File.t -> lreplay_results Lwt.t
end

let parse_line line =
  match String.split_on_char ',' line with
  | id :: status :: sl_tag :: origin_loc :: current_loc :: emitter :: drivers :: exec_time :: _ ->
      let sl_id = int_of_string (String.trim id) in
      let sl_status =
        match String.trim status with
        | "covered" ->
            let drivers =
              let filenames = String.split_on_char ' ' (String.trim drivers) in
              List.map (Format.sprintf "%s.c") filenames
            in
            Sc_C.Types.Covered drivers
        | "uncoverable" ->
            Uncoverable
        | "unknown" ->
            Unknown
        | s ->
            Fmt.failwith "Utils.status_of_string %s" s
      in
      let sl_orig_loc = Sc_C.Location.of_string (String.trim origin_loc)
      and sl_loc =      Sc_C.Location.of_string (String.trim current_loc)
      in                                          (* Set by the luncov plugin *)

      let sl_emitter =
        let emm = String.trim emitter in
        if emm = "" then None
        else Some emm
      in
      let sl_exec = float_of_string (String.trim exec_time) in
      if sl_id <= 0 then
        failwith (Format.asprintf
                    "Internal assumption not met: label id = %d \
                     (should be strictly positive)" sl_id);
      ignore sl_tag;
      Sc_C.Types.S {
        cov_label_id = sl_id;
        cov_label_status = sl_status;
        cov_label_emitter = sl_emitter;
        cov_label_exec_time = sl_exec;
        cov_label_loc = sl_loc;
        cov_label_orig_loc = sl_orig_loc;
      }
  | _ -> failwith ("TODO: Error on line: " ^ line)

let is_a_comment_line line = line = "" || line.[0] = '#'

let fold
    (label_file: [> `labeldb] Sc_sys.File.t)
    (f: Sc_C.Cov_label.simple -> 'a -> 'a)
    (init : 'a)
  : 'a =
  Log.debug "Reading file %s" (Sc_sys.File.name label_file);
  let< chan = label_file in
  try
    let rec aux acc =
      try
        let line = input_line chan in
        if is_a_comment_line line
        then aux acc
        else
          let lbl = parse_line line in
          aux (f lbl acc)
      with
        End_of_file -> close_in chan; acc
    in aux init
  with e -> close_in chan; raise e


let seq_filter file filter =
  List.rev @@ fold file (fun po acc -> if filter po then po :: acc else acc) []

let get_lbls ?(filter = fun _ -> true)  label_file =
  let simpl = seq_filter label_file filter in
  let hyper = [] in
  let any =
    List.map Sc_C.Cov_label.as_any simpl @
    List.map Sc_C.Cov_label.as_any hyper
  in
  {simpl; hyper; any}

module Make (Conf: CONFIG) : S = struct

  module Log_lwt_luncov = (val Ez_logs.subproc "luncov")

  let initialize (labelized_file : [> `C | `labelized] Sc_sys.File.t)  =
    (* Adds the location on the label database. *)
    let* luncov_init_cmd =
      Framac.luncov_init (module Conf) [labelized_file]
    in
    Sc_sys.Process.get_promise @@
    Sc_sys.Process.exec luncov_init_cmd
      ~stdout:(`Log Log_lwt_luncov.LWT.debug)
      ~stderr:(`Log Log_lwt_luncov.LWT.debug)
      ~on_success:(fun () -> Lwt.return ())

  let results (label_file : [> `labeldb] Sc_sys.File.t) =
    let lbls = seq_filter label_file (fun _ -> true) in
    let cov, unc, unk =
      List.fold_left begin fun (c, unc, unk) l ->
        let id = Sc_C.Cov_label.id l in
        match Sc_C.Cov_label.status l with
        | Covered _ ->
            Ints.add id c, unc, unk
        | Uncoverable ->
            c, Ints.add id unc, unk
        | Unknown ->
            c, unc, Ints.add id unk
      end (Ints.empty, Ints.empty, Ints.empty) lbls
    in
    Lwt.return {
      lreplay_covered = cov;
      lreplay_uncoverable = unc;
      lreplay_unknown = unk;
      lreplay_labels = lbls;
    }

end
