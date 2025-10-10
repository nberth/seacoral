(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Types

open Lwt.Syntax

module type S = sig
  val create_annotated_file
    : ?include_dirs:Sc_sys.File.dir list
    -> [> `C] Sc_sys.File.t
    -> Types.label_data Lwt.t
  val assume_annotated_file
    : [> `C] Sc_sys.File.t
    -> Types.label_data
end

(* Log module for reporting lannot sub-process output *)
module Log_lannot = (val Ez_logs.subproc "lannot")
module Log_luncov = (val Ez_logs.subproc "luncov")

let custom_labels_criteria =
  [
    "CUSTOM";
    "EMPTY";
  ]

let simple_labels_criteria =
  [
    "BC"; "CB"; "CC"; "DC"; "ELO";
    "FC"; "GACC"; "GICC"; "IDP"; "IOB";
    "LIMIT"; "MCC"; "NCC"; "RCC"; "SLO";
    "STMT"; "WM";
  ]

let hyper_labels_criteria =
  [
    "ADC"; "AUC"; "BADC"; "BAUC"; "BDUC";
    "CACC"; "DUC"; "FCC"; "RACC";
  ]

let supported_criteria = custom_labels_criteria @ simple_labels_criteria

(* Tool module for LAnnot.
   This tool is a little special: it is called at the beginning of the project
   and never after. It is not a tester, but the project initializer. *)
module Make (Conf : Types.CONFIG) : S = struct

  let labelized_file_name c_file =
    let basename = Sc_sys.File.basename ~chop_extension:true c_file in
    Sc_sys.File.name @@
    Sc_sys.File.PRETTY.assume_in ~dir:Conf.workspace.workdir
      "%s-annotated-for-%s.c" basename Conf.config.criterion

  let label_file_name c_file =
    Format.sprintf "%s.labels" @@
    Filename.chop_extension (labelized_file_name c_file)

  let lannot_cover_criterion =
    let crit = String.uppercase_ascii Conf.config.criterion in
    if List.mem crit custom_labels_criteria
    then "Empty"
    else if List.mem crit simple_labels_criteria
    then crit
    else if List.mem crit hyper_labels_criteria
    then raise @@ ERROR (Unsupported_criterion { crit; supported_criteria  })
    else raise @@ ERROR (Unknown_criterion { crit; supported_criteria })

  let create_annotated_file ?include_dirs (code_c: [> `C] Sc_sys.File.t) =
    (* Checking if the annoted_functions argument requires a specific
       computation. *)
    let annoted_functions: Framac.funs =
      match Conf.config.annotated_functions with
      | `Auto -> `Auto Conf.config.entrypoint
      | `All | `Only _ as funcs -> funcs
    and skipped_functions =               (* be sure not to annotate fixtures *)
      (if Conf.config.initialization_function <> ""
       then [Conf.config.initialization_function]
       else []) @
      (if Conf.config.oracle_function <> ""
       then [Conf.config.oracle_function]
       else [])
    in
    (* Actually ensures the label file also exists *)
    let labelized_file = labelized_file_name code_c in
    let* () =
      let* lannotcmd =
        Framac.lannot_cmd
          (module Conf)
          ?include_dirs
          ~cover_criterion:lannot_cover_criterion
          ~annoted_functions
          ~skipped_functions
          ~output_file:labelized_file
          code_c
      in
      Sc_sys.Process.get_promise @@
      Sc_sys.Process.exec lannotcmd
        ~stdout:(`Log Log_lannot.LWT.debug)
        ~stderr:(`Log Log_lannot.LWT.debug)
        ~on_success:(fun () -> Lwt.return ())
    in
    let labelized_file = Sc_sys.File.existing labelized_file in
    (* This updates the locations of the labels in the annoted C file. *)
    let* label_file =
      let* luncov_init_cmd =
        Framac.luncov_init (module Conf) [labelized_file]
      in
      let* () =
        Sc_sys.Process.get_promise @@
        Sc_sys.Process.exec luncov_init_cmd
          ~stdout:(`Log Log_luncov.LWT.debug)
          ~stderr:(`Log Log_luncov.LWT.debug)
          ~on_success:(fun () -> Lwt.return ())
      in
      Lwt.return @@ Sc_sys.File.existing (label_file_name code_c)
    in
    (* Replaces Frama-C builtins by their original value. *)
    Framac.remove_framac_builtins_from_file labelized_file;
    Lwt.return Types.{label_file; labelized_file}

  let assume_annotated_file (file: [> `C] Sc_sys.File.t) =
    let labelized_file = Sc_sys.File.existing @@ labelized_file_name file
    and label_file = Sc_sys.File.existing @@ label_file_name file in
    Types.{ labelized_file; label_file }
end

let check_availability () =
  (* Check that there's an lannot plugin, and that the installed version has
     option `-lannot-with-called`. *)
  let* ok =
    Framac.check_option "lannot-with-called" ~errlog:Log_lannot.LWT.debug
  in
  if not ok
  then raise @@ ERROR (Missing_frama_c_plugin { name = "lannotate" })
  else Lwt.return ()
