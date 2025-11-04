(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Sc_sys.File.Syntax
open Types

let percentage i j =
  Format.sprintf "%f%%" ((float_of_int (i * 100)) /. (float_of_int j))

(** Writes the content of a channel in a string. *)
let stringify_chan chan =
  let res = ref "" in
  try
    while true do
      res := !res ^ "\n" ^ input_line chan;
    done;
    assert false
  with
  | End_of_file -> !res

(* Each template has one or more identifiers to replace by some data. *)
type 'a replacer = {
  str: Str.regexp; (* The regexp to replace in the template *)
  replacer: 'a -> string; (* Gets the data *)
  once: bool; (* Does the tag appear once or more? *)
}

(* Builds the replacer *)
let replacer ?(once=true) str replacer = {
  str = Str.regexp_string str;
  replacer;
  once
}

(* Applies a replacer from a structuring data. *)
let apply_replacer t html r =
  let rep = if r.once then Str.replace_first else Str.global_replace in
  rep r.str (r.replacer t) html

(* The basic characteristics of a template file. *)
module type Template = sig
  type t
  val template_file: string
  val replacers: t replacer list
end

module type ToHtml = sig
  type t
  val to_html : t -> string
  val to_fhtml : Format.formatter -> t -> unit
end

module RawHtml = struct
  type t = string
  let to_html = Fun.id
  let to_fhtml = Format.pp_print_string
end

(* Todo: merge as same type? *)
type 'content accordion = {
  acc_name: string;
  acc_id: string;
  acc_title: string;
  acc_content: 'content
}

type 'content tab = {
  tab_name: string;
  tab_id: string;
  tab_title: string;
  tab_content: 'content
}

let make_accordion_list ~name l =
  let name =
    String.map (function
        | 'a'..'z'
        | 'A'..'Z'
        | '0'..'9' as c -> c
        | _ -> '-'
      ) name
  in
  List.mapi
    (fun i (title, c) -> {
         acc_name = name;
         acc_id = Format.sprintf "%s-%i" name i;
         acc_title = title;
         acc_content = c
       }
    )
    l

let make_tab_list ~name l =
  let name =
    String.map (function
        | 'a'..'z'
        | 'A'..'Z'
        | '0'..'9' as c -> c
        | _ -> '-'
      ) name
  in
  List.mapi
    (fun i (title, c) -> {
         tab_name = name;
         tab_id = Format.sprintf "%s-%i" name i;
         tab_title = title;
         tab_content = c
       }
    )
    l

(* Returns an untemplated version of the template in argument. *)
module PageBuilder (T: Template) : ToHtml with type t = T.t = struct

  type t = T.t

  let file_content =
    match Html_templates.read T.template_file with
    | None ->
       raise (RESOURCE_ERROR (Missing_resource_file {filename = T.template_file}))
    | Some f -> f

  let to_html t =
    List.fold_left
      (apply_replacer t)
      file_content
      T.replacers

  let to_fhtml fmt t = Format.fprintf fmt "%s" (to_html t)
end

(** Structuring the HTML report. *)

(** Properly builds an UL list. *)
module UL(Elt: ToHtml) = struct
  type t = Elt.t list

  let pp_elt_list fmt e = Format.fprintf fmt "<li>%a</li>" Elt.to_fhtml e

  let to_fhtml fmt t =
    Format.fprintf fmt "<ul>%a</ul>" (Format.pp_print_list pp_elt_list) t

  let to_html = Format.asprintf "%a" to_fhtml
end

(** An accordion HTML element, never use outside a list. *)
module AccordionTpl(ContentBuilder: ToHtml)
  : Template with type t = ContentBuilder.t accordion = struct
  type t = ContentBuilder.t accordion

  let template_file = "accordion.html.tpl"

  let replacers : t replacer list = [
    replacer "__ACC_NAME__" (fun t -> t.acc_name);
    replacer ~once:false "__ACC_ID__" (fun t -> t.acc_id);
    replacer "__ACC_TITLE__" (fun t -> t.acc_title);
    replacer "__ACC_CONTENT__" (fun t -> ContentBuilder.to_html t.acc_content);
  ]
end

(** A list of accordions. *)
module AccordionListTpl (ContentBuilder: ToHtml)
  : ToHtml with type t = ContentBuilder.t accordion list =
  PageBuilder (struct
    module Accordion = PageBuilder(AccordionTpl(ContentBuilder))

    type t = Accordion.t list

    let template_file = "accordion_list.html.tpl"

    let replacers : t replacer list = [
      replacer
        "__ACCORDIONS__"
        (fun l ->
           List.fold_left
             (fun acc accord -> acc ^ Accordion.to_html accord)
             ""
             l
        )
    ]
  end
  )

(** A single tab, never use outside a list. *)
module TabTpl(ContentBuilder: ToHtml) : Template with type t = ContentBuilder.t tab = struct
  type t = ContentBuilder.t tab

  let template_file = "tab.html.tpl"

  let replacers : t replacer list = [
    replacer "__TAB_NAME__" (fun t -> t.tab_name);
    replacer ~once:false "__TAB_ID__" (fun t -> t.tab_id);
    replacer "__TAB_TITLE__" (fun t -> t.tab_title);
    replacer "__TAB_CONTENT__" (fun t -> ContentBuilder.to_html t.tab_content);
  ]
end

(** A list of tabs. *)
module TabListTpl (ContentBuilder: ToHtml) : ToHtml with type t = ContentBuilder.t tab list =
  PageBuilder (struct
    module Tab = PageBuilder(TabTpl(ContentBuilder))

    type t = ContentBuilder.t tab list

    let template_file = "tab_list.html.tpl"

    let replacers : t replacer list = [
      replacer
        "__TABS__"
        (fun l ->
           List.fold_left
             (fun acc tab -> acc ^ Tab.to_html tab)
             ""
             l
        )
    ]
  end
  )

(** Now, we can create the different structures. *)

(** Prints a configuration line. *)
module ToolConfig = UL(struct
    type t = string * string (* Key * value *)

    let to_fhtml fmt (key, v) = Format.fprintf fmt "%s: %s" key v

    let to_html t = Format.asprintf "%a" to_fhtml t
  end)

(** A run configuration is displayed as an config accordion. *)
module RunConfig = AccordionListTpl(ToolConfig)

(** The runs template of the overview. *)
module RunTpl = struct
  type t = {
    run_time: float;
    run_config: RunConfig.t;
    extra_run_html: string (* Raw HTML for now *)
  }

  let template_file = "run.html.tpl"

  let replacers : t replacer list = [
    replacer "__RUN_TIME__" (fun t -> string_of_float t.run_time);
    replacer
      "__RUN_CONFIG_ACCORDIONS__"
      (fun t -> RunConfig.to_html t.run_config);
    replacer "__EXTRA_RUN_STATISTICS__" (fun t -> t.extra_run_html);
  ]
end

(** A single run. *)
module SingleRun = PageBuilder(RunTpl)

module Runs = TabListTpl(SingleRun)

(** The overview page.  *)
module OverviewTpl = struct
  type t = {
    num_tests_gen: int;
    num_tests_imported: int;
    num_labels: int;
    num_covered: int;
    num_uncoverable: int;
    num_runs: int;
    runs: Runs.t
  }

  let num_unknown t = t.num_labels - t.num_covered - t.num_uncoverable

  let template_file = "overview.html.tpl"

  let replacers : t replacer list = [
    replacer "__TESTS_GENERATED__"
      (fun t -> string_of_int (t.num_tests_gen + t.num_tests_imported));
    (* TODO: add __TESTS_IMPORTED__ *)
    replacer "__NUM_LABELS__" (fun t -> string_of_int t.num_labels);
    replacer "__COVERED_LABELS__" (fun t -> string_of_int t.num_covered);
    replacer
      "__COVERED_PERCENTAGE__"
      (fun t -> percentage t.num_covered t.num_labels);
    replacer
      "__UNCOVERABLE_LABELS__"
      (fun t -> string_of_int t.num_uncoverable);
    replacer
      "__UNCOVERABLE_PERCENTAGE__"
      (fun t -> percentage t.num_uncoverable t.num_labels);
    replacer
      "__UNKNOWN_LABELS__"
      (fun t -> string_of_int (num_unknown t));
    replacer
      "__UNKNOWN_PERCENTAGE__"
      (fun t -> percentage (num_unknown t) t.num_labels);
    replacer "__NUM_RUNS__" (fun t -> string_of_int t.num_runs);
    replacer "__OVERVIEW_RUN_TABS__" (fun t -> Runs.to_html t.runs)
  ]
end

module Overview = PageBuilder(OverviewTpl)

module SplitTestTpl = struct
  type t = {
    tool_name: string;
    tool_time: float;
    test_file: [`C] Sc_sys.File.t;
  }

  let template_file = "test.html.tpl"

  let c_file_to_html =
    let lt = Str.regexp_string "<" in
    let gt = Str.regexp_string ">" in
    let backslash = Str.regexp_string "\\" in
    fun cfile ->
      let< c= cfile in
      c
      |> stringify_chan
      (* Character \0, \1, ... has a meaning in Str, we add to each '\'
         the empty HTML character '&#8203;'. *)
      |> Str.global_replace backslash "\\&#8203;"
      (* Additional replacement for HTML compatibility. *)
      |> Str.global_replace lt "&lt"
      |> Str.global_replace gt "&gt"

  let replacers : t replacer list = [
    replacer
      "__TOOL_NAME__"
      (fun t -> Format.sprintf "Generated by %s" t.tool_name);
    replacer
      "__TOOL_TIME__"
      (fun t -> string_of_float t.tool_time);
    replacer
      "__TEST__"
      (fun t -> c_file_to_html t.test_file)
  ]
end

module SplitTest = PageBuilder(SplitTestTpl)

module SplitTests = TabListTpl(SplitTest)

module CombinedTestsTpl = struct
  type t = {
    file: [`C] Sc_sys.File.t;
  }
  let template_file = "combined_tests.html.tpl"

  let c_file_to_html =
    let lt = Str.regexp_string "<" in
    let gt = Str.regexp_string ">" in
    let backslash = Str.regexp_string "\\" in
    fun cfile ->
      let< c= cfile in
      c
      |> stringify_chan
      (* Character \0, \1, ... has a meaning in Str, we add to each '\'
         the empty HTML character '&#8203;'. *)
      |> Str.global_replace backslash "\\&#8203;"
      (* Additional replacement for HTML compatibility. *)
      |> Str.global_replace lt "&lt"
      |> Str.global_replace gt "&gt"

  let replacers : t replacer list = [
    replacer
      "__TESTS__"
      (fun t -> c_file_to_html t.file)
  ]
end

module CombinedTests = PageBuilder(CombinedTestsTpl)

module CombinedTestsTabs = TabListTpl(CombinedTests)

module LabelTpl = struct
  type t = {
    static_db_data: Sc_C.Cov_label.simple;
    runtime_db_data: [`Cov | `Uncov | `Unk];
  }

  let template_file = "label.html.tpl"

  module UL = UL(RawHtml)

  let descr t =
    Format.asprintf
      "Label %i, in %a"
      (Sc_C.Cov_label.id t.static_db_data)
      Sc_C.Location.print (Sc_C.Cov_label.location t.static_db_data)

  let details t =
    let details_list =
      (* TODO: use drivers information *)
      let status =
        match Sc_C.Cov_label.status t.static_db_data, t.runtime_db_data with
        | Covered _, `Uncov
        | Uncoverable, `Cov ->
            "<span style='color:red'>Inconsistent result</span>"
        | Covered [], _ ->
            "Covered"
        | Covered l, _ ->
            Format.asprintf
              "Covered by %a"
              (Basics.PPrt.pp_lst ~fsep:"," Format.pp_print_string) l
        | _, `Cov -> "Covered"
        | Uncoverable, _
        | _, `Uncov ->
            "Uncoverable"
        | Unknown, `Unk ->
            "Unknown"
      in
      let covered_by = None in (* TODO *)
      let emitter = Sc_C.Cov_label.emitter t.static_db_data in
      [Format.sprintf "Status: %s" status] @
      begin
        match covered_by with
        | None -> []
        | Some test -> [Format.sprintf "Covered by: %s" test]
      end @
      begin
        match emitter with
        | None -> []
        | Some e -> [Format.sprintf "Emitter: %s" e]
      end
    in
    UL.to_html details_list


  let replacers = [
    replacer "__LABEL_DESCRIPTION__" descr;
    replacer "__DETAILS__" details
  ]
end

module Label = PageBuilder(LabelTpl)

module Labels = TabListTpl(Label)

module MainReportTab = struct
  type t =
    | Overview of Overview.t
    | Tests of tests
    | Labels of Labels.t

  and tests =
    | Split of SplitTests.t
    | Combined of CombinedTestsTabs.t

  let to_fhtml fmt = function
    | Overview o -> Overview.to_fhtml fmt o
    | Tests Split t -> SplitTests.to_fhtml fmt t
    | Tests Combined t -> CombinedTestsTabs.to_fhtml fmt t
    | Labels l -> Labels.to_fhtml fmt l


  let to_html = function
    | Overview o -> Overview.to_html o
    | Tests Split t -> SplitTests.to_html t
    | Tests Combined t -> CombinedTestsTabs.to_html t
    | Labels l -> Labels.to_html l
end

module MainReportTabs = TabListTpl(MainReportTab)

module ReportTpl = struct
  type t = {
    r_title: string;
    r_tabs: MainReportTabs.t;
  }

  let template_file = "index.html.tpl"

  let replacers = [
    replacer ~once:false "__COVERAGE_REPORT_TITLE__" (fun t -> t.r_title);
    replacer "__REPORT_TABS__" (fun t -> MainReportTabs.to_html t.r_tabs);
  ]
end

module Report = PageBuilder(ReportTpl)

(* Defining type aliases to simplify the external interface. *)
type tool_config = (string * string) list

type run_config = tool_config accordion list

type run = RunTpl.t = {
  run_time: float;
  run_config: run_config;
  extra_run_html: string
}

type run_tabs = run tab list

type overview = OverviewTpl.t = {
  num_tests_gen: int;
  num_tests_imported: int;
  num_labels: int;
  num_covered: int;
  num_uncoverable: int;
  num_runs: int;
  runs: run_tabs
}

type test = SplitTestTpl.t = {
  tool_name: string;
  tool_time: float;
  test_file: [`C] Sc_sys.File.t;
}

type combined_tests = CombinedTestsTpl.t = {
  file: [`C] Sc_sys.File.t;
}

type tests = MainReportTab.tests =
  | Split of test tab list
  | Combined of combined_tests tab list

type label = LabelTpl.t = {
  static_db_data: Sc_C.Cov_label.simple;
  runtime_db_data: [`Cov | `Uncov | `Unk]
  (* The runtime database only has this information. *)
}

type report_tab = MainReportTab.t =
  | Overview of overview
  | Tests of tests
  | Labels of label tab list

type report = ReportTpl.t = {
  r_title: string;
  r_tabs: report_tab tab list;
}
