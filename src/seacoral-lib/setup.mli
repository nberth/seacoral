(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Project initialization *)

(** [project ~clean_start ~salt user_config] validates a user-given
    configuration and initializes a project.  This notably involves creating the
    associated working directory, if it does not exist already.

    May raise {!Types.ERROR}. *)
val project
  : ?clean_start: bool
  -> salt: Digest.t
  -> Types.config
  -> Sc_project.Types.project_config Lwt.t

val test_encoding_params
  : Types.config
  -> Sc_values.encoding_params Lwt.t
