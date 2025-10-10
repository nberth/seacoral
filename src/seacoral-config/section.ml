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
        Logs.Src.create "Sc_config.Section"
          ~doc:"Logs of configuration sections manager"))

type 'a section =
  {
    section_name: string;
    section_schema: 'a Eztoml.schema;
    section_default: 'a;
    mutable section_status: 'a section_status;
  }
and 'a section_status =
  | Registered
  | Configured of { values: 'a }
and any_section =
  | Any: _ section -> any_section

let sections: (string, any_section) Hashtbl.t =
  Hashtbl.create 1

let define (type o) name ~entries ~default : o section =
  if Hashtbl.mem sections name
  then raise @@ Errors.Duplicate_section_names name
  else
    let section =
      {
        section_name = name;
        section_schema = Eztoml.build entries;
        section_default = default;
        section_status = Registered;
      }
    in
    Hashtbl.add sections name (Any section);
    section

let section_subtable section table =
  let open Toml.Types in
  let key = Table.Key.of_string section.section_name in
  match Table.find key table with
  | TTable t ->
      t
  | value ->
      raise @@ Errors.Bad_value_type_in_toml_table (key, value, "table")
  | exception Not_found ->
      Table.empty

let update_section section from (table: Toml.Types.table) : _ =
  let error_header ppf =
    Fmt.pf ppf "Error@ in@ section@ [%s]:" section.section_name
  in
  try
    let subtable = section_subtable section table in
    Ok (Eztoml.parse from subtable section.section_schema)
  with
  | Errors.Bad_value_type_in_toml_table  _
  | Errors.Bad_value_in_toml_table _ as e ->
      Basics.PPrt.string_to (fun s -> Error s) "@[<2>%t@;%a@]"
        error_header Fmt.exn e
  | Errors.Unknown_toml_key _ as e ->
      Basics.PPrt.string_to (fun s -> Error s)
        "@[<2>%t@;%a@;(accepted@ entries@ are:@;@[%a@])@]"
        error_header Fmt.exn e
        Fmt.(list ~sep:comma string) (Eztoml.keys section.section_schema)

let load_section section ?from (table: Toml.Types.table) : (unit, string) result =
  let from =
    match from, section.section_status with
    | Some s, _ -> s
    | None, Configured { values } -> values
    | None, Registered -> section.section_default
  in
  match update_section section from table with
  | Ok values ->
      section.section_status <- Configured { values };
      Ok ()
  | Error e ->
      Error e

let load (table: Toml.Types.table) : (unit, string) result =
  (* A local exception to stop the iteration. The string is the Error
     message to return. *)
  (* TODO: symbolic errors *)
  let exception Err of string in
  try
    Hashtbl.iter begin fun _name (Any section) ->
      match load_section section table with
      | Ok () -> ()
      | Error e -> raise (Err e)
    end sections;
    Ok ()
  with Err e ->
    Error e

let get (type o) ?(check_loaded = true) (section: o section) : o =
  match section.section_status with
  | Registered when check_loaded ->
      raise @@ Errors.Unconfigured_section section.section_name
  | Registered ->
      Log.warn "Configuration section %S has not been loaded (yet?); returning \
                default options." section.section_name;
      section.section_default
  | Configured { values; _ } ->
      values

let core_digest () : Digest.t =
  let buff = Buffer.create 42 in
  Hashtbl.iter begin fun _name (Any section) ->
    Buffer.add_string buff @@
    Digest.to_hex @@
    Eztoml.core_digest section.section_schema @@
    match section.section_status with
    | Registered -> section.section_default
    | Configured { values } -> values
  end sections;
  Digest.bytes (Buffer.to_bytes buff)

let print_toml_spec ppf (Any section) : unit =
  Eztoml.print_as_toml_file ppf (section.section_name, section.section_schema)

let section_name (Any s) =
  s.section_name

let section_compare s1 s2 =
  String.compare (section_name s1) (section_name s2)

let iter_sections ~head f =
  (* Small hack to visit important modules first. *)
  let in_head s = List.exists (fun s' -> section_compare s s' = 0) head in
  List.iter f head;
  (* Sort remaining sections lexicographically. *)
  List.iter f begin
    List.sort section_compare @@ List.of_seq @@
    Seq.filter (fun s -> not (in_head s)) @@
    Hashtbl.to_seq_values sections
  end

let print_default_config_file ~head ppf =
  iter_sections ~head (print_toml_spec ppf)

let print_current_config_file ppf =
  iter_sections ~head:[] begin fun (Any section) ->
    let value =
      match section.section_status with
      | Registered -> None
      | Configured {values; _} -> Some values
    in
    Eztoml.print_as_toml_file ~with_doc:false ?value ppf
      (section.section_name, section.section_schema)
  end

let print_doc ~head ppf =
  iter_sections ~head begin fun (Any section) ->
    let section_name = Fmt.(styled (`Fg (`Hi `Magenta)) string) in
    let section_header ppf section =
      Fmt.pf ppf "@[<h>Configuration@ section@ [%a]:@]"
        section_name section.section_name
    in
    Fmt.pf ppf "@[<v>%a@;%a@]@."
      Fmt.(styled `Bold section_header) section
      Eztoml.print_doc section.section_schema
  end

let key_prefix ?(with_section_name_prefix = true) section =
  if with_section_name_prefix
  then Some section.section_name
  else None

let manpage_section_name (Any section) =
  Eztoml.manpage_section_name section.section_name

let as_section_update_cmdliner_term ?with_section_name_prefix section =
  Eztoml.as_section_update_cmdliner_term section.section_schema
    ~section_name:section.section_name
    ?prefix:(key_prefix ?with_section_name_prefix section)

let as_toml_table_cmdliner_term sections =
  List.fold_left begin fun table (Any s, prefix_flag) ->
    let with_section_name_prefix = prefix_flag = `with_section_name_prefix in
    Eztoml.acc_toml_table_for_cmdliner s.section_schema table
      ~section_name:s.section_name
      ?prefix:(key_prefix ~with_section_name_prefix s)
  end (Cmdliner.Term.const Toml.Types.Table.empty) sections
