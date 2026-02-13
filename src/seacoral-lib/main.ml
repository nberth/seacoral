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
open Sc_core.Types
open Sc_project.Types

open Lwt.Infix
open Lwt.Syntax
open Sc_sys.File.Syntax

(* Everything happening here is application related: *)
module Log = Main_log

(* --- *)

type generation_options =
  {
    run: run_config;
    enable_detailed_stats: bool;
    strategy: Sc_strategy.Types.t;
    print_statistics: bool;
  }

type replay_options =
  {
    replay_config: run_config;
  }

(* --- *)

(** {2 Raw statistics printers} *)

module CSV = struct
  let pp_header ?(sep: Basics.PPrt.ufmt = "\t") ppf =
    Fmt.(fmt "%a" ppf (list ~sep:(any sep) string))
      ["covered"; "uncoverable"; "unknown"; "num_tests_gen"]

  let pp_stats ?(sep: Basics.PPrt.ufmt = "\t") : Sc_project.Types.info Fmt.t =
    Fmt.using
      begin fun (Sc_store.Types.{ covered_ids; uncoverable_ids; num_ids },
                 Sc_corpus.Types.{ num_tests_gen; _ }) ->
        let c = Basics.Ints.cardinal covered_ids
        and u = Basics.Ints.cardinal uncoverable_ids in
        [c; u; num_ids -u -c; num_tests_gen]
      end @@ Fmt.(list ~sep:(any sep) uint)
end

(** Display outcome table *)
let display_test_outcomes ~options
    (type raw_test) ~(project: raw_test Sc_project.Types.project) =
  match options.run.display_outcomes with
  | `No ->
      Lwt.return ()
  | `Yes | `Verbose as v ->
      let module Raw_test = (val project.params.test_repr) in
      let pp_test_id ppf (t: raw_test Sc_corpus.Types.test_view) =
        match v with
        | `Verbose ->
            Raw_test.Val.print ppf (Lazy.force t.raw)
        | `Yes ->
            Fmt.pf ppf "%i" t.metadata.serialnum
      in
      let* tests = Lwt_stream.to_list @@ Sc_corpus.existing_tests project.corpus in
      List.sort Sc_corpus.compare_tests_by_serialnum tests |>
      Lwt_list.iter_s begin fun t ->
        Log.LWT.app "Test %a: %a"
          pp_test_id t
          Sc_corpus.Printer.pp_test_outcome t.metadata.outcome
      end

let retrieve_and_log_statistics ~options ~project =
  let toc = Unix.gettimeofday () in
  let info_file = project.config.project_workspace.workdir / "covinfo.csv" in
  let* exists = Sc_sys.Lwt_file.exists info_file in
  let* (cov, corpus_info) as info = Sc_project.Manager.info project in
  let>> oc = info_file in
  let ppf = Format.formatter_of_out_channel oc in
  if not exists
  then Fmt.fmt "# run\ttime\tfunction\t%t@\n" ppf (CSV.pp_header ~sep:"\t");
  let covered = cov.covered_ids in
  let uncoverable = cov.uncoverable_ids in
  Fmt.fmt "%u\t%f\t%s\t%a@." ppf
    project.config.project_run.run_num
    (Float.max 0. (toc -. Sc_project.Run.ref_time project.config.project_run))
    (Sc_project.Manager.entrypoint_name project)
    (CSV.pp_stats ~sep:"\t") info;
  if options.enable_detailed_stats then begin
    (* Display outcomes only when detailed stats are enabled. *)
    let* () = display_test_outcomes ~options ~project in
    Log.app "@[Coverage@ statistics@ for@ `%a':@;%a@]"
      Sc_project.Printer.pp_entrypoint_name project
      Sc_project.Printer.pp_coverage_info info;
    Log.app "@[Covered@ labels:@;%a@]" Basics.Ints.print covered;
    Log.app "@[Uncoverable@ labels:@;%a@]" Basics.Ints.print uncoverable;
    Log.app "@[Crash@ statistics:@;%a@]" Sc_project.Printer.pp_crash_info info;
    if Sc_project.Manager.seeks_oracle_failures project
    then Log.app "@[Oracle@ statistics:@;%a@]\
                 " Sc_project.Printer.pp_oracle_failures_info info;
    Lwt.return info
  end else begin
    (* Only show info on overall success; mostly for CI tests that are hard to
       make deterministic. *)
    Log.app "@[Simplified@ coverage@ results@ for@ `%a':@]"
      Sc_project.Printer.pp_entrypoint_name project;
    if corpus_info.num_tests_gen > 0
    then Log.app "@[- Tests@ were@ generated@]"
    else Log.app "@[- NO@ test@ was@ generated@]";
    if Basics.Ints.is_empty covered
    then Log.app "@[- NO@ label@ was@ covered@]";
    if not (Basics.Ints.is_empty uncoverable)
    then Log.app "@[- Some@ labels@ are@ uncoverable@]";
    if Sc_corpus.has_crashes (snd info)
    then Log.app "@[- Some@ crashes@ where@ found@]";
    if Sc_corpus.has_oracle_failures (snd info)
    then Log.app "@[- Some@ oracle@ failures@ where@ found@]"
    else if Sc_project.Manager.seeks_oracle_failures project
    then Log.app "@[- NO@ oracle@ failure@ was@ found@]";
    Lwt.return info
  end

let show_lreplay_results res =                     (* simple display, for now *)
  Log.app "%a" Sc_ltest.Printer.pp_lreplay_results (res, One_per_line)

    (* --- *)

let crosscheck_with_lreplay_results ~project
    ~(lreplay_results: Sc_ltest.Types.lreplay_results)
    ~(store_cov_info: Sc_store.Types.covinfo) =
  let lreplay = lreplay_results and cov_info = store_cov_info in
  if Basics.Ints.equal lreplay.lreplay_covered cov_info.covered_ids
  then Lwt.return ()
  else
    let lreplay_only
      = Basics.Ints.diff lreplay.lreplay_covered cov_info.covered_ids
    and store_only
      = Basics.Ints.diff cov_info.covered_ids lreplay.lreplay_covered
    in
    Log.warn "The@ set@ of@ covered@ labels@ reported@ by@ Lreplay@ does@ not@ \
              match@ SeaCoral's@ own@ accounting";
    if not (Basics.Ints.is_empty lreplay_only)
    then Log.warn "@[<2>- Reported@ covered@ by@ LReplay@ only:@;%a@]\
                  " Basics.Ints.print lreplay_only;
    if not (Basics.Ints.is_empty store_only)
    then Log.warn "@[<2>- Reported@ covered@ in@ the@ store@ only:@;%a@]\
                  " Basics.Ints.print store_only;
    (* TODO: when test->covered labels info will be available, we may filter
       what we print below. *)
    let* tests =
      Sc_corpus.existing_tests project.corpus |>
      Lwt_stream.filter Sc_corpus.is_covering_test |>
      Lwt_stream.to_list
    in
    Log.LWT.warn "@[<v>@[Label-covering@ tests:@]@;%a@]"
      (Sc_project.Printer.pp_tests ~project) tests

(* --- *)

let make_test_repr_module encoding_params =
  (module Sc_values.Struct.Repr (struct
       let encoding_params = encoding_params
     end): Sc_values.Struct.REPR)

(* --- *)

let generation_error err =
  raise @@ GENERATION_ERROR err

let generate ~project_config ~encoding_params (options: generation_options) =

  let initialization_options =
    Sc_core.Types.{
      force_preprocess = options.run.force_preprocess;
      inhibit_store_auto_termination = false;
    }
  in

  Log.app "Initializing@ working@ environment...";

  let module Raw_test = (val make_test_repr_module encoding_params) in
  let* project =
    Lwt.catch begin fun () ->
      Sc_project.Manager.initialize ~initialization_options
        ~test_repr:(module Raw_test)
        ~config:project_config
    end begin function
      | SETUP_ERROR e ->
          generation_error @@ Project_setup_error e
      | LABELING_ERROR e ->
          generation_error @@ Project_labeling_error e
      | ELABORATION_ERROR e ->
          generation_error @@ Project_elaboration_error e
      | e ->
          Lwt.reraise e
    end
  in

  (* Now, doing the hard work. *)
  let* testsuite =
    let to_check = project.labels in
    if not (Sc_project.Manager.seeks_oracle_failures project) &&
       List.filter Sc_C.Cov_label.is_unknown to_check.any = []
    then
      Log.LWT.app "Skipping@ `%a'@ (no@ objective@ left@ undecided)"
        Sc_project.Printer.pp_entrypoint_name project >>= fun () ->
      Sc_project.Export.assume_testsuite project
    else begin
      let* () =
        if project_config.project_run.run_num > 1 then begin
          let* info = Sc_project.Manager.info project in
          Log.LWT.app "Current@ coverage@ statistics@ for@ `%a':@;\
                       @[<v>@[%a@]@;@[%a@]@]"
            Sc_project.Printer.pp_entrypoint_name project
            Sc_project.Printer.pp_coverage_info info
            Sc_project.Printer.pp_crash_info info
        end else Lwt.return ()
      in

      Log.app "Doing the hard work...";
      let* () =
        Log.info "Orchestration@ strategy:@;%a\
                 " Sc_strategy.Printer.print options.strategy;
        Strategy.play ~project to_check options.strategy
          ~options:initialization_options
      in
      let* () = Sc_corpus.stop_receiving_tests project.corpus in
      Log.app "Extracting@ new@ testcases@ from@ corpus...";
      Sc_project.Export.write_testsuite project
    end
  in

  Log.app "Hard work done";

  let* cov_info, _ = retrieve_and_log_statistics ~options ~project in

  let* () =
    Sc_postproc.Lreplay.run project testsuite >>= function
    | None ->
        Lwt.return ()
    | Some lreplay_results ->
        if options.enable_detailed_stats
        then show_lreplay_results lreplay_results
        else Log.app "Skipped reporting of lreplay results";

        crosscheck_with_lreplay_results ~project ~lreplay_results
          ~store_cov_info:cov_info
  in

  (* E-ACSL *)
  let* () =
    Sc_postproc.Eacsl.run project testsuite >>= function
    | Some stats when options.enable_detailed_stats ->
        Log.LWT.app "Tests@ satisfying@ E-ACSL@ specification:@ %i/%i"
          stats.satisfied stats.total
    | Some stats when stats.total > 0 && stats.satisfied = 0 ->
        Log.LWT.app "NO test@ satisfies@ the@ E-ACSL@ specification"
    | Some stats when stats.total = stats.satisfied ->
        Log.LWT.app "Every test@ satisfies@ the@ E-ACSL@ specification"
    | Some stats when stats.total > 0 ->
        Log.LWT.app "Some tests@ satisfy@ the@ E-ACSL@ specification"
    | Some _                             (* total = 0: no test was considered *)
    | None ->
        Lwt.return ()
  in

  (* lcov *)
  let* () =
    Sc_postproc.Lcov.run ~project testsuite >>= function
    | Some dir ->
        Log.LWT.app "Generated@ LCOV-report@ in@ `%a'" Sc_sys.File.print dir
    | None ->
        Lwt.return ()
  in


  (* Generating the report. *)
  let* () =
    Lwt.catch begin fun () ->
      let* html = Sc_report.Main.generate_report project testsuite in
      match html with
      | Some f ->
          Log.LWT.app "Generated@ customized@ report@ in@ `%a'\
                      " Sc_sys.File.print f
      | None ->
          Lwt.return ()
    end begin function
      | Sc_report.Types.(CONFIG_ERROR _ | RESOURCE_ERROR _) as exn ->
          Log.err "Customized@ report@ generation@ aborted:@ %s\
                  " (Printexc.to_string exn);
          Lwt.return ()
      | e ->
          Lwt.reraise e
    end
  in

  (* Statistics. *)

  Sc_project.Manager.save_tool_statistics project;
  if options.print_statistics then
    Log.app "Statistics:@;@[<v 0>%a@]\
            " Sc_project.Manager.print_tool_statistics project;

  Lwt.return ()

let replay ~project_config ~encoding_params (options: replay_options) =

  if options.replay_config.force_preprocess then begin
      raise Types.(REPLAY_ERROR Cannot_force_preprocess)
    end;

  let initialization_options =
    Sc_core.Types.{
      force_preprocess = options.replay_config.force_preprocess;
      inhibit_store_auto_termination = true;
    }
  in

  Log.app "Initializing@ working@ environment...";

  let module Raw_test = (val make_test_repr_module encoding_params) in
  let* project =
    Lwt.catch begin fun () ->
      Sc_project.Manager.initialize ~initialization_options
        ~test_repr:(module Raw_test)
        ~config:project_config
    end begin function
      | SETUP_ERROR e ->
          generation_error @@ Project_setup_error e
      | LABELING_ERROR e ->
          generation_error @@ Project_labeling_error e
      | ELABORATION_ERROR e ->
          generation_error @@ Project_elaboration_error e
      | e ->
          Lwt.reraise e
    end
  in

  let* validator = Sc_corpus.Validator.setup project.validator in
  
  (* Revalidation of tests *)
  Log.app "Replaying@ tests.";
  let corpus = Sc_corpus.existing_tests ~sort_by_serial_num:true project.corpus in
  let test_w_outcomes =
    Lwt_stream.map_s
      (fun (test : Raw_test.Val.t Sc_corpus.Types.test_view) ->
        let* outcome =
          Sc_corpus.Validator.validate_raw_test validator (Lazy.force test.raw)
        in
        Lwt.return (test, outcome))
      corpus
  in
  Lwt_stream.iter_s
    (fun ((test: Raw_test.Val.t Sc_corpus.Types.test_view), outcome) ->
      match outcome with
      | None ->
         Log.LWT.err "Validator@ returned@ no@ outcome@ for@ test %i;@ \
                      expected@ outcome %a"
           test.metadata.serialnum
           Sc_corpus.Printer.pp_test_outcome test.metadata.outcome
      | Some o ->
         let* () =
           Log.LWT.app
             "Outcome@ for@ test@ %i:@ %a."
             test.metadata.serialnum
             Sc_corpus.Printer.pp_test_outcome o
         in
         if o <> test.metadata.outcome then
           Log.LWT.err "Expected@ outcome:@ %a.\
                       " Sc_corpus.Printer.pp_test_outcome test.metadata.outcome
         else Lwt.return ()
    )
    test_w_outcomes
