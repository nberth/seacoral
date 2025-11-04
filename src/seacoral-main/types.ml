(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Sc_sys.File.TYPES

type logs_config =
  {
    log_level: Logs.level option;
    (** Log level. *)
  }

(** Command-line-only options *)
type options =                                (* TODO: split per sub-command  *)
  {
    clean_start: bool;
    (** When set, start by archiving the work done on this project. *)

    config_file: [`toml] file option;
    (** Configuration file given on the command-line. *)

    amending_table: Toml.Types.table;
    (** Amendments to the configuration that come from command-line
        arguments. *)

    print_statistics: bool;
    (** When set, print statistics at the end of the run. *)
  }
