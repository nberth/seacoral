(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** A manager for manipulating toml files, used for configuring the
    orchestrator. *)

module TYPES: sig

  type toml_file =
    {
      file: [`toml] Sc_sys.File.file;
      toml_contents: string;
    }

end

type file = TYPES.toml_file

type 'c row

open Eztoml_helpers

(** {2 Definitions}

    [row_def_with_default_in_doc] bindings must come with a documentation string
    that must specify the default value.  The default value is only provided for
    documentation purposes, and for generating a default project configuration
    file.

    Use [row_def_without_default_in_doc] when showing a default value is not
    relevant for documentation. *)
type ('a, 'c) row_def_with_default_in_doc =
  ('a, ('c -> 'a -> 'c) -> ('c -> 'a) -> 'c row) def_with_default_in_doc
type ('a, 'c) row_def_without_default_in_doc =
  ('a, ('c -> 'a -> 'c) -> ('c -> 'a) -> 'c row) def_without_default_in_doc

(** Describes how Boolean rows appear when on the command line.  Defaults to
    [Positive {keys = `Same}] for both {!bool} and {!bool'}.

    The name of the flag on the command-line is influenced by the [keys] fiels
    in inline records (when available, depending on constructors): when it is
    [`Same], the name of the row is reused; when it is [`Alt alternative_keys],
    [alternative_keys] are reused instead.  The same holds for documentation
    with [pos_doc] and [neg_doc] fields in the case of [Both]. *)
type cmdline_flag_type =
  | Valued       (** leads to one flag with a mandatory value [--key=BOOLEAN] *)
  | Positive of
      {
        keys: [`Same | `Alt of string list];
        doc:  [`Same | `Alt of string];
      }             (** leads to a single flag that sets the option to [true] *)
  | Negative of
      {
        keys: [`Same | `Alt of string list];
        doc:  [`Same | `Alt of string];
      }            (** leads to a single flag that sets the option to [false] *)
  | Both of
      {
        pos_keys: [`Same | `Alt of string list];
        pos_doc:  [`Same | `Alt of string];
        neg_keys: [`Same | `Alt of string list];
        neg_doc:  [`Same | `Alt of string];
      } (** leads to two flags, one with key(s) [pos_keys] that sets the option
            to [true], the other with key(s) [neg_keys] that resets the option.
            [pos_doc] and [neg_doc] describe the documentation associated with
            the positive and negative flags, respectively.

            [pos_keys = neg_keys = `Same], or otherwise non-disjoint sets of
            keys, leads to undefined behaviors (likely an error in {!Cmdliner},
            or one of the flags being silently ignored). *)

val unit: (unit, 'c) row_def_with_default_in_doc
val bool: ?as_flag: cmdline_flag_type -> (bool, 'c) row_def_with_default_in_doc
val int: (int, 'c) row_def_with_default_in_doc
val float: (float, 'c) row_def_with_default_in_doc
val string: (string, 'c) row_def_with_default_in_doc
val bool_list: (bool list, 'c) row_def_with_default_in_doc
val int_list: (int list, 'c) row_def_with_default_in_doc
val float_list: (float list, 'c) row_def_with_default_in_doc
val string_list: (string list, 'c) row_def_with_default_in_doc
val bool_dict: ((string * bool) list, 'c) row_def_with_default_in_doc
val int_dict: ((string * int) list, 'c) row_def_with_default_in_doc
val float_dict: ((string * float) list, 'c) row_def_with_default_in_doc
val string_dict: ((string * string) list, 'c) row_def_with_default_in_doc

val bool': ?as_flag: cmdline_flag_type -> (bool, 'c) row_def_without_default_in_doc
val int': (int, 'c) row_def_without_default_in_doc
val float': (float, 'c) row_def_without_default_in_doc
val string': (string, 'c) row_def_without_default_in_doc
val bool_list': (bool list, 'c) row_def_without_default_in_doc
val int_list': (int list, 'c) row_def_without_default_in_doc
val float_list': (float list, 'c) row_def_without_default_in_doc
val string_list': (string list, 'c) row_def_without_default_in_doc
val bool_dict': ((string * bool) list, 'c) row_def_without_default_in_doc
val int_dict': ((string * int) list, 'c) row_def_without_default_in_doc
val float_dict': ((string * float) list, 'c) row_def_without_default_in_doc
val string_dict': ((string * string) list, 'c) row_def_without_default_in_doc

(** {3 Definitions for values with some additional constraint} *)

type ('a, 'c) row_def_checked =
  ('a, ('c -> 'a -> 'c) -> ('c -> 'a) -> 'c row) checked_def_with_default_in_doc
val strictly_positive_int: (int, 'c) row_def_checked
val positive_int: (int, 'c) row_def_checked

(* --- *)

type 'config schema

val build : 'config row list -> 'config schema

val keys: _ schema -> string list

(** [parse section table section_schema] parses the TOML table [table] based on
    [section_schema], and updates the given [section] accordingly. *)
val parse: 'config -> Toml.Types.table -> 'config schema -> 'config

(** [core_digest section_schema section] computes a digest of the non-runtime
    part of the given configuration section. *)
val core_digest: 'config schema -> 'config -> Digest.t

(** Prints the schema's documentation. *)
val print_doc: Format.formatter -> _ schema -> unit

(** [print_as_toml_file ?with_doc ?value fmt (main_key, schema)]
    Prints the schema as a toml table under the key [main key].
    If [with_doc] is set to [true] ([true] by default), prints the documentation
    along the corresponding bindings.
    If [value] is provided, prints value instead of the schema default value. *)
val print_as_toml_file :
  ?with_doc:bool -> ?value: 'a ->
  Format.formatter -> string * 'a schema -> unit

val manpage_section_name: string -> string

(** [as_section_update_cmdliner_term ~prefix section_schema section_term]
    translates [section_schema] into a {!Cmdliner} term.  Each key of the schema
    leads to a distinct key (where '_'s are transformed in '-'s) and [prefix],
    if provided, prefixes each key.  For example, a key ["bar"] prefixed with
    ["foo"] will be associated with argument ["--foo-bar"] on the command
    line. *)
val as_section_update_cmdliner_term
  : ?prefix: string
  -> section_name: string
  -> 'config schema
  -> 'config Cmdliner.Term.t
  -> 'config Cmdliner.Term.t

(** [acc_toml_table_for_cmdliner ~prefix section_name section_schema table]
    translates [section_schema] into a {!Cmdliner} term that generates a TOML
    table for the corresponding section, based on command-line arguments.  See
    {!as_section_update_cmdliner_term} for details on [prefix]. *)
val acc_toml_table_for_cmdliner
  : ?prefix: string
  -> section_name: string
  -> 'config schema
  -> Toml.Types.table Cmdliner.Term.t
  -> Toml.Types.table Cmdliner.Term.t

(** {2 Parsing}

    TOML tables may be read from a string, or directly from a file. *)

(** [load_string ~filename toml] returns a TOML table as encoded in [toml], or
    an error.  [filename] is used in error reporting: any associated file is not
    actually read by this function. *)
val load_string
  : filename: string
  -> string
  -> (Toml.Types.table, Errors.toml_error) result

(** [read_file file] actually reads in the contents of [file] (which is assumed
    to exist, or else {!Sc_sys.File.MISSING} is raised), but does not parse it
    yet.  To be used in combination with {!load_file} for actual parsing. *)
val read_file: [`toml] Sc_sys.File.file -> file

(** [load_file file] returns a TOML table as encoded in [file], or an error. *)
val load_file: file -> (Toml.Types.table, Errors.toml_error) result
