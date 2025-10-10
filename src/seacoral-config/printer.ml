(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Errors

(** Prints a string giving a hint for fixing the configuration file. *)
let pp_config_fix ~error_line ~loc ~msg ppf =
  (* A source of failure come from duplicate keys. *)
  let str_rx = Str.regexp ".*add_value" in
  if Str.string_match str_rx msg 0 then
    Fmt.pf ppf "Key appears twice"
  else
    (* A possible source of errors comes from keys: if there is a
       forbidden character inside, it has to be between quotes. *)
  if loc.Toml.Parser.column >= String.length error_line then
    (* Happens when the toml library expect a token *)
    Fmt.pf ppf "Unexpected end of line, did you forget quotes?"
  else
    let error_char = error_line.[loc.column] in
    let is_error_char =
      match error_char with
      | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '-' ->
          (* This should be allowed, we stop here *)
          false
      | _ ->
          true
    in
    if is_error_char then
      Fmt.pf ppf "Did you forget quotes?@,"


(** Prints an invalid configuration file error as follows:
    "
    Error message
    Line with error
              ^^^^^
    Possible fix: a possible fix
    "
*)
let pp_toml_error ppf (TOML_error { toml; msg; loc }) =
  (* Getting the line with the error *)
  let error_line =
    List.nth (String.split_on_char '\n' toml) (loc.Toml.Parser.line - 1)
  in
  (* Creating the underline that we will fill with '^'s *)
  let err_len = String.length error_line in
  let col =
    if loc.column >= err_len then
      err_len - 1
    else
      loc.column
  in
  let underline =
    let empty_underline = Bytes.make (String.length error_line) ' ' in
    (* Iterates on the problematic line and adds an '^' to the underline
       while we don't get a delimiter character. *)
    let rec iter_on_line ~forward pos =
      match error_line.[pos] with
      | ' ' | '\x0C' | '\n' | '\r' | '\t'
      | '"' | '\'' | '[' | ']'
      | exception (Invalid_argument _) ->
          () (* We can stop underlining. *)
      | _ ->
          Bytes.set empty_underline pos '^';
          let next_pos = if forward then pos + 1 else pos - 1 in
          iter_on_line ~forward next_pos
    in
    (* Filling the underline *)
    iter_on_line ~forward:true col;
    iter_on_line ~forward:false col;
    Bytes.to_string empty_underline
  in
  (* Trying to guess the error *)
  Fmt.pf ppf
    "@[<v>%s\
     @,%a\
     @,%s\
     @,%t@]\
     @,"
    msg
    (Fmt.styled `Bold Fmt.string) error_line
    underline
    (pp_config_fix ~error_line ~loc ~msg)

let init () =
  Printexc.register_printer begin function
    | Unknown_toml_key k ->
        Some (Format.sprintf "Unknown toml key %S" k)

    | Bad_value_type_in_toml_table (key, value, typ) ->
        Some (
          Format.asprintf
            "Key `%s' is associated with %a; value of type %s expected"
            (Toml.Types.Table.Key.to_string key)
            Toml.Printer.value value
            typ)

    | Bad_value_in_toml_table { key; value; expect; pp_doc } ->
        Some (Fmt.str "@[<h>Value@ %s@ given@ for@ key@ `%s'%(%).@]@.\
                       @[<v 2>Documentation:@;@[%t@]@]"
                (* NB: `Toml.Printer.value` messes around with format boxes in a
                   weird way. *)
                (* Toml.Printer.value value *)
                (Toml.Printer.string_of_value value)
                (Toml.Types.Table.Key.to_string key) expect pp_doc)

    | Unconfigured_section s ->
        Some (Fmt.str "Internal error: configuration section %S was requested \
                       but has not been loaded yet.@;Hint for devs: \
                       `Sc_config.Section.get section` must be called AFTER \
                       `Sc_config.Section.load`" s)

    | Duplicate_section_names s ->
        Some (Fmt.str "Internal error: duplicate registration of configuration \
                       section %S" s)

    | Invalid_string_value { key; value; descr = None } ->
        Some (Fmt.str "Unknown value %S for configuration key %S" key value)

    | Invalid_string_value { key; value; descr = Some d } ->
        Some (Fmt.str "Unknown value %S for %s (configuration key %S)" key d value)

    | _ ->
        None
  end
