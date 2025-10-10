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
open Sc_core.Types
open Sc_project.Types
open Sc_strategy.Types

open Lwt.Infix
open Lwt.Syntax

module Log =
  (val (Ez_logs.from_src @@
        Logs.Src.create ~doc:"Logs of strategy runner" "Sc_lib.Strategy"))

(* --- *)

let check_tools_spec = function
  | ["*"] ->
      ()               (* accept wildcard (every tool — see below) *)
  | tools ->
      match List.filter (fun tool_name -> not (Tool.is_known tool_name)) tools with
      | [] -> ()
      | tools -> raise @@ CONFIG_ERROR (Unknown_tools (NEL.of_list tools))

let make tools_spec strategy =
  let tool_list =
    match tools_spec with
    | ["*"] -> Tool.known_tool_names ()
    | tool_names -> tool_names
  in
  let tool_list =
    try NEL.of_list tool_list
    with Invalid_argument _ -> raise @@ CONFIG_ERROR Missing_tools
  in
  match strategy with
  | AllParallel ->
      Parallel (NEL.map ~f:(fun t -> Tool t) tool_list)
  | AllSequential ->
      Sequence (NEL.map ~f:(fun t -> Tool t) tool_list)
  | Custom s ->
      s
  | Optimized ->
      let preproc, dynamic, static, unknowns =
        NEL.fold_left ([], [], [], []) tool_list ~f:begin fun (p, d, s, u) tool ->
          match Tool.find tool with
          | exception Not_found -> (p, d, s, tool :: u)
          | { tool_kind = `Preprocess; _ } -> (Tool tool :: p, d, s, u)
          | { tool_kind = `Dynamic; _ } -> (p, Tool tool :: d, s, u)
          | { tool_kind = `Static; _ } -> (p, d, Tool tool :: s, u)
        end
      in
      if unknowns <> [] then
        raise @@ CONFIG_ERROR (Unknown_tools (NEL.of_list unknowns));
      let parallel = function [] -> [] | l -> [ Parallel (NEL.of_list l) ] in
      let stages = parallel preproc @ parallel dynamic @ parallel static in
      Sc_strategy.normalize @@ Sequence (NEL.of_list stages)

(* --- *)

let preprocessing_error err =
  raise @@ TOOL_PREPROCESSING_ERROR err

let computation_error err =
  raise @@ TOOL_COMPUTATION_ERROR err

let failed_preprocessing ~toolname ~workdir_status exn =
  let backtrace = Printexc.get_raw_backtrace () in
  let error = { workdir_status; error = exn, backtrace } in
  preprocessing_error @@ Failed_preprocessing { tool = toolname; error }

let failed_computation ~toolname error =
  let backtrace = Printexc.get_raw_backtrace () in
  computation_error @@ Failed_computation { tool = toolname; error; backtrace }

let log_unexpected_errors: (unit -> 'a Lwt.t) -> 'a Lwt.t = fun f ->
  Main_log.LWT.err_if_rejected f ~silence:begin function
    | TOOL_PREPROCESSING_ERROR _ -> true
    | TOOL_COMPUTATION_ERROR _ -> true
    | _ -> false
  end

let catch_errors f =
  Lwt.catch f begin function
    | TOOL_PREPROCESSING_ERROR e ->
        raise @@ GENERATION_ERROR (Tool_preprocessing_error e)
    | TOOL_COMPUTATION_ERROR e ->
        raise @@ GENERATION_ERROR (Tool_computation_error e)
    | e ->
        Lwt.reraise e
  end

(* --- *)

let initialize_tool ~toolname ~options ~setup project =
  let failed_preprocessing = failed_preprocessing ~toolname in
  let workspace = Sc_core.Workspace.mksub project.workspace toolname in
  Main_log.debug "Initializing@ %s" toolname;
  log_unexpected_errors begin fun () ->
    Sc_sys.Lwt_lazy.persist_in ~dir:workspace.workdir
      ~force:options.force_preprocess
      ~donefile:".preproc-done"
      begin fun () ->
        Lwt.catch
          (fun () -> setup workspace ~optional:false @@ Tool_sigs.P project)
          (failed_preprocessing ~workdir_status:`Created)
      end
      begin fun () ->
        Lwt.catch
          (fun () -> setup workspace ~optional:true @@ Tool_sigs.P project)
          (failed_preprocessing ~workdir_status:`Reused)
      end
  end

let run_tool ~toolname run =
  log_unexpected_errors begin fun () ->
    Lwt.catch run (failed_computation ~toolname)
  end

let setup_tool ~toolname ~options project setup =
  catch_errors begin fun () ->
    Main_log.debug "Setting@ up@ %s..." toolname;
    let* run = initialize_tool ~toolname ~options ~setup project in
    Lwt.return begin fun () ->
      run_tool ~toolname run
    end
  end

let play ~project ~options (to_check: Sc_ltest.Types.labels) strat =
  ignore to_check;                                         (* ignored for now *)
  let ready_analyses = Hashtbl.create 1 in
  let rec setup = function
    | Nothing ->
        Lwt.return ()
    | Sequence sl | Parallel sl ->
        Lwt_list.iter_p setup @@ NEL.to_list sl
    | Tool toolname ->
        try
          let Tool.{ tool_setup; tool_availability_check; _ }
            = Tool.find toolname
          in
          let* ok = tool_availability_check () in
          if not ok                         (* TODO: accumulate symbolic errors *)
          then Main_log.LWT.err "Tool@ %S@ is@ not@ available" toolname
          else begin
            Hashtbl.add ready_analyses toolname =|<
            setup_tool ~toolname ~options project tool_setup
          end
        with Not_found ->
          Main_log.LWT.err "Unknown@ tool@ %S" toolname
  in
  let* () = setup strat in
  Main_log.info "Setup@ of@ tools@ done;@ now@ launching@ the@ analyses...";
  let rec aux ?(top = true) : Sc_strategy.Types.t -> _ = function
    | Nothing ->
        Lwt.return ()
    | Sequence sl ->
        let sl = NEL.to_list sl in
        log_step sl `S ~top >>= fun () -> Lwt_list.iter_s (aux ~top:false) sl
    | Parallel sl ->
        let sl = NEL.to_list sl in
        log_step sl `P ~top >>= fun () -> Lwt_list.iter_p (aux ~top:false) sl
    | Tool toolname as s ->
        log_step [s] `S ~top >>= fun () ->
        (* Traveresed in setup, therefore present in [ready_analyses] *)
        catch_errors (Hashtbl.find ready_analyses toolname)
  and log_step sl kind ~top =
    if top then           (* only at top-level for clarity of application log *)
      Main_log.LWT.app "Launching@ %a@ %aon@ `%a'"
        (Basics.PPrt.with_oxford_comma Sc_strategy.Printer.print) sl
        (match kind with
         | `P | `S when List.length sl <= 1 -> Fmt.nop
         | `P -> Fmt.any "in@ parallel@ "
         | `S -> Fmt.any "in@ sequence@ ") ()
        Sc_project.Printer.pp_entrypoint_name project
    else
      Lwt.return ()
  in
  aux strat
