(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Gathers various operations needed when preprocessing C projects *)

val codebase_file_in
  : dir:Sc_sys.File.dir
  -> Types.project_config
  -> [`C] Sc_sys.File.t

val gather_codebase_in
  : dir:Sc_sys.File.dir
  -> Types.project_config
  -> [ `C ] Sc_sys.File.t Lwt.t

(** May raise {!LABELING_ERROR} (with {!Syntax_errors} payload) *)
val do_syntax_check
  : incdir:Sc_sys.File.dir
  -> config:Types.project_config
  -> [< `C | `CXX ] Sc_sys.File.t
  -> unit Lwt.t

(** Symbol used to replace `main` in manipulated source code. *)
val main_replacement_symbol: string

(** Alter the configuration to make sure the entrypoint is not called "main". *)
val patch_entrypoint_name
  : Types.project_config
  -> Types.project_config

val setup_for
  : config:Types.project_config
  -> test_repr:'raw_test Sc_corpus.Types.repr
  -> c_file:[ `C | `labelized ] Sc_sys.File.t
  -> 'raw_test Types.project_params Lwt.t
