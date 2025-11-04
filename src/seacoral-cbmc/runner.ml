(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Json interface for CBMC. *)

open Types
open DATA

open Lwt.Syntax
open Sc_sys.Lwt_file.Syntax

type 'a process_result = 'a

(* --- *)

let log_src = Logs.Src.create ~doc:"Logs of CBMC caller" "Sc_cbmc.Runner"
module Log = (val (Ez_logs.from_src log_src))

type _ exec_kind =
  | GetProperties : property list cell list exec_kind
  | GetCoverObjectives : property list cell list exec_kind
  | GetCLabels : Sc_C.Cov_label.simple list -> property list cell list exec_kind
  | CoverAnalysis : [`simple] analysis_env -> cbmc_cover_output cell list exec_kind
  | AssertAnalysis : [`simple] analysis_env -> cbmc_assert_output cell list exec_kind
  | CLabelAnalysis : [`simple] analysis_env -> cbmc_assert_output cell list exec_kind

let pp_execution_kind (type k) : k exec_kind Fmt.t = fun ppf e ->
  Fmt.string ppf @@ match e with
  | GetProperties -> "get-properties"
  | GetCoverObjectives -> "get-objectives"
  | GetCLabels _ -> "get-clabels"
  | CoverAnalysis _ -> "cover-analysis"
  | AssertAnalysis _ -> "assert-analysis"
  | CLabelAnalysis _ -> "clabels-analysis"

let _source_location_to_location (sloc : source_location) : Sc_C.Types.location =
  try { loc_line = int_of_string sloc.slline;
        loc_file = Filename.basename sloc.slfile }
  with Failure s -> Fmt.failwith "Source location to location failed: %s" s

let property_belongs_to_file ~file prop =
  Filename.basename prop.psource_location.slfile =
  Sc_sys.File.basename file

let id_from_property_name prop =
  match String.split_on_char '.' prop.pname with
  | [fname; kind; id] -> fname, kind, int_of_string id
  | _ -> assert false

let empty_env =
  { extra_required_properties = [];
    proof_objectives = PropertyMap.empty;
    already_proven = PropertyMap.empty }

let property_kind_matches_mode ~mode ~kind =
  match mode with
  | OPTIONS.Cover -> kind = "coverage"
  | Assert -> kind = "assertion"
  | CLabel -> kind = "error_label"

(* Takes the list of properties returned by cbmc with the option --show-properties and
   returns the associated proof objectives (the labels to cover). *)
let uncovered_properties
    ~mode
    ~harness_file
    ~labelized_file
    ~(cbmc_props : property list)
    ~(already_decided : Basics.Ints.t)
    ~labels
    ~entrypoint
  : [`simple] analysis_env =
  (* Sorting labels in a map for quick search *)
  let lbl_map =
    Basics.IntMap.of_seq @@                     (* of_list requires OCaml≥5.1 *)
    Seq.map (fun l -> Sc_C.Cov_label.id l, l) @@
    List.to_seq labels
  in
  let env =
    List.fold_left begin fun env prop ->
      (* Log.debug "Property %a" Printer.pp_property prop; *)
      let fname, kind, lbl_id = id_from_property_name prop in
      Log.debug "Property@ %s.%i@ of@ kind@ %s" fname lbl_id kind; 
      (* Checking if the property belongs to the main file and the main function. *)
      if not (property_kind_matches_mode ~mode ~kind) then
        { env with
          extra_required_properties = prop :: env.extra_required_properties }
      else if property_belongs_to_file ~file:harness_file prop && fname = entrypoint then
        (* Probably unsafe *)
        match Basics.IntMap.find_opt lbl_id lbl_map with
        | None ->                                               (* not a label *)
            { env with
              extra_required_properties = prop :: env.extra_required_properties }
        | Some lbl when
               Sc_C.Cov_label.is_unknown lbl &&
               not (Basics.Ints.mem lbl_id already_decided) ->(* unknown status *)
            { env with
              proof_objectives = PropertyMap.add prop lbl env.proof_objectives }
        | Some lbl ->                                          (* known status *)
            { env with
              already_proven = PropertyMap.add prop lbl env.already_proven }
      else if property_belongs_to_file ~file:labelized_file prop then
        { env with
          extra_required_properties = prop :: env.extra_required_properties }
      else begin
        (* We discard properties that are not in the entrypoint function. *)
        Log.debug "Discarding@ property@ %s:@;source@ files@ differ@ \
                   (@[%s@ <>@ %s)@]"
          prop.pname
          prop.psource_location.slfile
          (Sc_sys.File.name labelized_file);
        env
      end
    end empty_env cbmc_props
  in
  SimpleLabelEnv env

let cbmc_generic_process
    ~resdir
    ~timeout
    ~(inputs_json : [>`json] Sc_sys.File.t)
    ~(outputs_json : [>`json] Sc_sys.File.t)
    ~(errors_file : _ Sc_sys.File.t)
    ~(store : Sc_store.t) : _ result Lwt.t =
  let* inputs_fd =
    Log.debug "input: `%a'" Sc_sys.File.print inputs_json;
    Sc_sys.Lwt_file.descriptor inputs_json [O_RDONLY] 0       (* perm. unused *)
  and* outputs_fd =
    Log.debug "output: `%a'" Sc_sys.File.print inputs_json;
    Sc_sys.Lwt_file.descriptor outputs_json [O_WRONLY; O_CREAT; O_TRUNC] 0o600
  and* errors_fd =
    Log.debug "errors: `%a'" Sc_sys.File.print errors_file;
    Sc_sys.Lwt_file.descriptor errors_file [O_WRONLY; O_CREAT; O_TRUNC] 0o600
  in
  let* proc =
    Sc_sys.Process.exec
      Sc_sys.Ezcmd.Std.(make "cbmc" |>
                        key "json-interface" |>
                        rawf "-I%a" Sc_sys.File.print resdir |>
                        to_cmd)
      ~stdin:(`FD_move (Lwt_unix.unix_file_descr inputs_fd))
      ~stdout:(`FD_move (Lwt_unix.unix_file_descr outputs_fd))
      ~stderr:(`FD_move (Lwt_unix.unix_file_descr errors_fd))
      ~timeout
      ~on_success:Lwt.return_ok
      ~on_error:Lwt.return_error
  in
  let* _cancel_kill =
    Sc_store.on_termination store ~h:(fun _ -> Sc_sys.Process.terminate proc)
  in
  Sc_sys.Process.join proc

(* From the lannot label identifier, returns the corresponding error label for CBMC *)
let label_of pp s = Format.asprintf "sc_label%a" pp s (* Defined in cbmc_label_driver.h *)

let sc_opt_to_opt
    ?oproperties
    ?(oshow_properties=false)
    ?oerror_label
    ?ocover
    ~ofunction
    ~files
    sc_opt =
  let oproperties =
    match oproperties with
    | None -> None
    | Some {proof_objectives; extra_required_properties; _} ->
        let prop_names =
          (* Concat names of proof objectives and extra props *)
          let po_names = PropertyMap.names proof_objectives in
          List.fold_left
            (fun po_names {pname; _} -> pname :: po_names)
            po_names
            extra_required_properties
        in
        Some prop_names
  in
  {
    oarguments = List.map Sc_sys.File.absname files;
    ofunction;
    ounwind = if sc_opt.OPTIONS.unwind = 0 then None else Some sc_opt.unwind;
    oproperties;
    oshow_properties;
    ocover;
    oerror_label;
    opointer_check = true;
    onondet_static = false;
    omalloc_may_fail = CantFail;
  }

let error_label_of_simple_lbl (l: Sc_C.Cov_label.simple) : string =
  label_of Format.pp_print_int (Sc_C.Cov_label.id l)

let label_of_property p = error_label_of_simple_lbl p

let opt_encoding_and_cmd_options_from_exec_kind
    (type a) (ek : a exec_kind)
    (ofunction : string)
    (files : [`C] Sc_sys.File.t list)
    (sc_opt : OPTIONS.t) : json_options * a Json_encoding.encoding =
  match ek with
  | GetProperties ->
      sc_opt_to_opt
        ~ofunction
        ~oshow_properties:true
        ~files
        sc_opt,
      Json.Output.property_data

  | GetCoverObjectives ->
      sc_opt_to_opt
        ~ofunction
        ~oshow_properties:true
        ~ocover:"cover"
        ~files
        sc_opt,
      Json.Output.property_data

  | GetCLabels lbls ->
      sc_opt_to_opt
        ~ofunction
        ~oshow_properties:true
        ~files
        ~oerror_label:(List.map error_label_of_simple_lbl lbls)
        sc_opt,
      Json.Output.property_data

  | CoverAnalysis (SimpleLabelEnv oproperties) ->
      sc_opt_to_opt
        ~oproperties
        ~ofunction
        ~ocover:"cover"
        ~files
        sc_opt,
      Json.Output.cover_analysis_output

  | AssertAnalysis (SimpleLabelEnv oproperties) ->
      sc_opt_to_opt
        ~oproperties
        ~ofunction
        ~files
        sc_opt,
      Json.Output.assert_analysis_output

  | CLabelAnalysis (SimpleLabelEnv oprops) ->
      let oerror_label =
        PropertyMap.fold
          (fun _ p acc -> label_of_property p :: acc)
          oprops.proof_objectives
          []
      in
      sc_opt_to_opt
        ~ofunction
        ~files
        ~oerror_label
        sc_opt,
      Json.Output.assert_analysis_output



let write_json ek ~runner_options (options : json_options) : [`json] Sc_sys.File.t Lwt.t =
  let json = Json.options options in
  let file =
    Sc_sys.File.PRETTY.assume_in ~dir:runner_options.runner_inputs
      "%u-%a-options.json" runner_options.runner_iteration pp_execution_kind ek
  in
  let* () = Sc_sys.Lwt_file.write file json in
  Lwt.return file

let out_json ek ~runner_options : [`json] Sc_sys.File.t Lwt.t =
  Lwt.return @@
  Sc_sys.File.PRETTY.assume_in ~dir:runner_options.runner_outputs
    "%u-%a-outputs.json" runner_options.runner_iteration pp_execution_kind ek

let err_file ek ~runner_options : [`stderr] Sc_sys.File.t Lwt.t =
  Lwt.return @@
  Sc_sys.File.PRETTY.assume_in ~dir:runner_options.runner_outputs
    "%u-%a-errors" runner_options.runner_iteration pp_execution_kind ek

let err_json ek ~runner_options : [`stderr] Sc_sys.File.t Lwt.t =
  Lwt.return @@
  Sc_sys.File.PRETTY.assume_in ~dir:runner_options.runner_outputs
    "%u-%a-json-error.json" runner_options.runner_iteration pp_execution_kind ek

let read_json_result ~outputs_json ~encoding ~errors_file ~errors_json_file =
  let log_err json =
    let* () =
      let<* ec = errors_file in
      Lwt_stream.iter_s (Log.LWT.err "stderr: %s") @@ Lwt_io.read_lines ec
    in
    Sc_sys.Lwt_file.write errors_json_file json
  in
  Lwt.catch begin fun () ->
    let* json_string = let<* ic = outputs_json in Lwt_io.read ic in
    Lwt.return @@ Json.read_cbmc_output encoding json_string
    end begin function
      | (FAILED_JSON_PARSING {exn = _; json} as e) ->
         Log.err "Error while parsing CBMC's output";
         let* () = log_err json in
         Lwt.reraise e
      | (FAILED_JSON_DESTRUCT {exn = _; json} as e) ->
         Log.err "Error while destructing CBMC's output";
         let* () = log_err json in
         Lwt.reraise e
      | e -> 
         Log.err "Unexpected error while reading CBMC's output";
         Lwt.reraise e
  end

let cbmc_start
    (type a)
    (ek : a list exec_kind)
    ~(runner_options: runner_options)
    ~(silent_kill: bool)
    ~(store : Sc_store.t)
    ~(entrypoint : string)
    ~(files : [`C] Sc_sys.File.t list) (options : OPTIONS.t)
  : a list Lwt.t =
  let joptions, encoding =
    opt_encoding_and_cmd_options_from_exec_kind ek entrypoint files options
  in
  let* inputs_json = write_json ek ~runner_options joptions
  and* outputs_json = out_json ek ~runner_options
  and* errors_file = err_file ek ~runner_options
  and* errors_json_file = err_json ek ~runner_options in
  let* status =
    cbmc_generic_process ~resdir:runner_options.runner_resdir ~store
      ~timeout:options.timeout ~inputs_json ~outputs_json ~errors_file
  in
  match status with                        (* Note: already reported, in logs *)
  | Error (Unix.WSIGNALED -7) when silent_kill ->               (* Manual kill *)
      Lwt.return []
  | _ ->
      read_json_result ~outputs_json ~encoding ~errors_file ~errors_json_file

let cbmc_get_properties ~store ~runner_options ~entrypoint ~files opt =
  cbmc_start GetProperties ~store ~runner_options ~entrypoint ~files opt
    ~silent_kill:true

let cbmc_get_cover_objectives ~store ~runner_options ~entrypoint ~files opt =
  cbmc_start GetCoverObjectives ~store ~runner_options ~entrypoint ~files opt
    ~silent_kill:true

let cbmc_get_clabels ~store ~lbls ~runner_options ~entrypoint ~files opt =
  cbmc_start (GetCLabels lbls) ~store ~runner_options ~entrypoint ~files opt
    ~silent_kill:true

let cbmc_cover_analysis ~store ~runner_options ~entrypoint ~files ~to_cover opt =
  cbmc_start (CoverAnalysis to_cover) ~store ~runner_options ~entrypoint ~files opt
    ~silent_kill:false

let cbmc_assert_analysis ~store ~runner_options ~entrypoint ~files ~to_cover opt =
  cbmc_start (AssertAnalysis to_cover) ~store ~runner_options ~entrypoint ~files opt
    ~silent_kill:false

let cbmc_clabel_analysis ~store ~runner_options ~entrypoint ~files ~to_cover opt =
  cbmc_start (CLabelAnalysis to_cover) ~store ~runner_options ~entrypoint ~files opt
    ~silent_kill:false
