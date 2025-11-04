(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Run initialization, with persistence of configuration *)

open Types
open Sc_config.Eztoml.TYPES

open Lwt.Infix
open Lwt.Syntax
open Sc_sys.File.Syntax
open Sc_sys.Lwt_file.Syntax

(* --- *)

let actual_toml_fmt: (int -> 'a, _, _, _, _, 'a) format6 = "%u-actual.toml"

let run_num_of ~file = (* raises Scanf.Scan_failure if filename does not match *)
  Scanf.sscanf (Sc_sys.File.basename file) actual_toml_fmt Fun.id

(** [dump_config_in ~workdir ?given_config_file run_num] dumps configuration
    files and command-line arguments in a dedicated directory under
    [workdir]. *)
let dump_config_in ~workdir ?given_config_file run_num =
  let dir = workdir / "config" in
  let* () = Sc_sys.Lwt_file.touch_dir dir in
  let write_given () =
    let>* oc = Sc_sys.File.PRETTY.assume_in ~dir "%u-given.toml" run_num in
    match given_config_file with
    | Some i -> Lwt_io.write oc i.toml_contents
    | None -> Lwt.return ()
  and write_cmdline () =
    let>* oc = Sc_sys.File.PRETTY.assume_in ~dir "%u-cmdline.txt" run_num in
    Lwt_io.write oc @@                                  (* one item per line: *)
    Basics.PPrt.to_string "%a" Fmt.(vbox @@ array ~sep:cut @@ string) Sys.argv
  and write_actual () =
    let>* oc = Sc_sys.File.PRETTY.assume_in ~dir actual_toml_fmt run_num in
    Lwt_io.write oc @@
    Basics.PPrt.to_string "%t" Sc_config.Section.print_current_config_file
  in
  Lwt.join [write_given (); write_cmdline (); write_actual ()]

(** [actual_toml_files_in ~workdir] lists every TOML file where actual
    configuration is saved upon new runs in [workdir]. *)
let actual_toml_files_in ~workdir =
  let extract_run_num file =
    try Some (run_num_of ~file, file) with Scanf.Scan_failure _ -> None
  and compare_config_ids (i, _) (j, _) =
    Int.compare i j
  in
  Sc_sys.Lwt_file.files_of_dir (workdir / "config") |>
  Lwt_stream.filter_map extract_run_num |>
  Lwt_stream.to_list >|= fun files ->
  List.sort compare_config_ids files |>
  List.map snd

(** Dump the configuation, and retrieve persisted run data. *)
let new_in ~workdir ~loaded_config_file:given_config_file =
  let meta_file = workdir / ".scdata" in
  let run_num, run_ref_times =
    if Sc_sys.File.exists meta_file
    then
      let< ic = meta_file in
      let num = input_binary_int ic in
      num, List.init num (fun _ -> (input_value ic : float))
    else
      0, []
  in
  let run_num = succ run_num in
  (let> oc = meta_file in output_binary_int oc run_num);
  let* meta_stat = Sc_sys.Lwt_file.stat meta_file in
  let run_ref_times = NEL.cons_list meta_stat.st_mtime run_ref_times in
  (let>> oc = meta_file in NEL.iter ~f:(output_value oc) run_ref_times);
  let* () = dump_config_in ~workdir ?given_config_file run_num in
  let* run_configs = actual_toml_files_in ~workdir in
  Lwt.return { run_num; run_ref_times; run_configs }

(** [successive_configs run] returns a stream of TOML tables with actual run
    configuration for each successive run before and including [run]. *)
let successive_configs run : Toml.Types.table Lwt_stream.t =
  let { run_configs; _ } = run in
  Lwt_stream.of_list run_configs |>
  Lwt_stream.filter_map_s begin fun file ->
    let* toml = let<* ic = file in Lwt_io.read ic in
    Lwt.return @@
    Result.to_option @@                                     (* discard errors *)
    Sc_config.Eztoml.load_string ~filename:(Sc_sys.File.name file) toml
  end

(* ref time of current run *)
let ref_time run =
  NEL.hd run.run_ref_times

(* TODO: catch invalid run_num *)
let ref_time_of ~run_num run =
  List.nth (NEL.rev_to_list run.run_ref_times) (run_num - 1)
