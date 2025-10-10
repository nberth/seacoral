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

let log_level_of_int = function
  | 0 -> None
  | 1 -> Some Logs.App
  | 2 -> Some Logs.Error
  | 3 -> Some Logs.Warning
  | 4 -> Some Logs.Info
  | _ -> Some Logs.Debug

let int_of_log_level = function
  | None -> 0
  | Some Logs.App -> 1
  | Some Logs.Error  -> 2
  | Some Logs.Warning -> 3
  | Some Logs.Info -> 4
  | Some Logs.Debug -> 5

let logs_section =
  let default: logs_config =
    {
      log_level = Some Logs.Warning;
    }
  in
  Sc_config.Section.define "logs" ~default ~entries:Sc_config.Eztoml.[
      int
        ~key:"console-level"                      (* TODO: use an `enum` kind *)
        ~arg_alias:["log-level"; "debug"]
        ~doc:"Console log level: use 0 for no message, 1 for application-level \
              messages only, 2 to include error messages, 3 to include warnings \
              as well, 4 for more information, and 5 or any other value for full \
              debug messages (default: %a)."
        ~default:(int_of_log_level default.log_level)
        ~runtime:true
        (fun _ l -> { log_level = log_level_of_int l })
        (fun c -> int_of_log_level c.log_level);
    ]

let default: options =
  {
    clean_start = false;
    config_file = None;
    amending_table = Toml.Types.Table.empty;
    print_statistics = false;
  }

module Parsing = struct
  open Cmdliner

  let docs = Manpage.s_common_options

  let clean_start_term config_term =
    let keys = ["clean-start"] in
    let doc =
      "Remove the project with the same hash before starting its analysis \
       (default: false)"
    in
    let v = Arg.(value & flag & info keys ~doc ~docs) in
    let set c f = {c with clean_start = f} in
    Term.(map set config_term $ v)

  let config_file_term config_term =
    let keys = ["config"; "C"; "configuration-file"] in
    let doc = "Use the given configuration file" in
    let v = Arg.(value & opt (some Arg.non_dir_file) None &
                 info keys ~doc ~docv:"FILE" ~docs) in
    let set c f = { c with config_file = Option.map Sc_sys.File.assume f } in
    Term.(map set config_term $ v)

  let config_amendments_term ?(amendable_sections = []) config_term =
    let table_term =
      Sc_config.Section.as_toml_table_cmdliner_term amendable_sections
    in
    let set table c = { c with amending_table = table } in
    Term.(map set table_term $ config_term)

  let statistics config_term =
    let keys = ["show-statistics"; "stats"] in
    let doc = "Print statistics of the current run" in
    let v = Arg.(value & flag & info keys ~doc ~docs) in
    let set c f = {c with print_statistics = f} in
    Term.(map set config_term $ v)

  let config_term ?amendable_sections config =
    Term.const config
    |> clean_start_term
    |> statistics
    |> config_file_term
    |> config_amendments_term ?amendable_sections

end

let term ~config_sections_that_show_up_as_arguments:amendable_sections =
  Parsing.config_term ~amendable_sections default
