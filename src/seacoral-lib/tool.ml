(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

module Log =
  (val (Ez_logs.from_src @@
        Logs.Src.create ~doc:"Logs of Tool interface" "Sc_lib.Tool"))

type tool =
  {
    tool_setup: (unit -> unit Lwt.t) Tool_sigs.setup;
    tool_config: Sc_config.Section.any_section option;
    tool_labels: targeted_labels;
    tool_availability_check: unit -> bool Lwt.t;
    tool_kind: [`Preprocess | `Static | `Dynamic];
  }

and targeted_labels =
  | For_simple_labels
  | For_any_label

(* A table regrouping all tools defined with the database *)
let tools: (string, tool) Hashtbl.t =
  Hashtbl.create 3

let register ~name ~kind ~config_section ~availability_check tool_labels
    tool_setup =
  let tool_config =
    match config_section with
    | None -> None
    | Some s -> Some (Sc_config.Section.Any s)
  in
  (* TODO: check for duplicates *)
  Hashtbl.replace tools name {
    tool_setup;
    tool_config;
    tool_labels;
    tool_availability_check = availability_check;
    tool_kind = kind;
  }

let register_module labels (module T: Tool_sigs.WITH_PARAMETRIC_PROJECT) =
  register labels begin fun ws ~optional (P p) ->
    Lwt.bind (T.setup ws ~optional p) @@ fun ready_generator ->
    Lwt.return @@ fun () -> T.run ready_generator
  end

let register_module_with_boxed_project labels (module T: Tool_sigs.WITH_BOXED_PROJECT) =
  register labels begin fun ws ~optional p ->
    Lwt.bind (T.setup ws ~optional p) @@ fun ready_generator ->
    Lwt.return @@ fun () -> T.run ready_generator
  end

let register_functions labels ~setup ~run =
  register labels begin fun ws ~optional p ->
    Lwt.bind (setup ws ~optional p) @@ fun ready_generator ->
    Lwt.return @@ fun () -> run ready_generator
  end

let register_function labels ~setup =
  register labels setup

let find name : tool =
  Hashtbl.find tools name

let is_known name =
  Hashtbl.mem tools name

let known_tool_names () =
  Hashtbl.to_seq_keys tools |>
  List.of_seq

let known_config_sections () =
  Hashtbl.to_seq_values tools |>
  Seq.filter_map (fun tool -> tool.tool_config) |>
  List.of_seq
