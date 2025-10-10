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

let section_name = "report"

let invalid_config_value ~key ~value ~possible_values =
  raise (CONFIG_ERROR (Invalid_config_value {key; value; possible_values}))

let missing_resource_file ~filename =
  raise (RESOURCE_ERROR (Missing_resource_file {filename}))

let section =
  let default =
    {
      enable = false;
      report_format = `Pretty;
      output_dir = "report";
    }
  and parse_kind s = match String.lowercase_ascii s with
    | "pretty" -> `Pretty
    | "onepage" -> `OnePage
    | _ ->
       invalid_config_value
         ~key:"format"
         ~value:s
         ~possible_values:["pretty"; "onepage"]
  and kind_to_string = function
    | `Pretty -> "pretty"
    | `OnePage -> "onepage"
  in
  Sc_config.Section.define section_name ~default ~entries:Sc_config.Eztoml.[
      bool
        ~key:"enable"
        ~doc:"Generates an HTML report (default: %a)"
        ~default:default.enable
        ~as_flag:(Positive { keys = `Alt ["report"]; doc = `Same })
        ~runtime:true
        (fun c enable -> {c with enable})
        (fun c -> c.enable);
      string
        ~key:"format"
        ~doc:"Selects whether the report must be in a single page (\"onepage\") \
              or with tabs (\"pretty\") (default: %a)"
        ~default:(kind_to_string default.report_format)
        ~runtime:true
        (fun c e -> {c with report_format = parse_kind e})
        (fun c -> kind_to_string c.report_format);
      string
        ~key:"output-dir"
        ~doc:"Output directory (default: %a)"
        ~default:default.output_dir
        ~runtime:true
        (fun c od -> {c with output_dir = od})
        (fun c -> c.output_dir);
    ]

let css_file c =
  let file =
    match c.report_format with
    | `Pretty -> "css/style.css"
    | `OnePage -> "css/style_one_page.css"
  in
  match
    Html_templates.read file with
  | Some file_content -> file_content
  | None -> missing_resource_file ~filename:file
