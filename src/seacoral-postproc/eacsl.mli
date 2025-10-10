(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

module TYPES: sig

  type options =
    {
      eacsl_enabled: bool;
      eacsl_mode: eacsl_postproc;
      eacsl_gcc_path: string;
    }
  and eacsl_postproc =
    | Sequential
    | Parallel

  type eacsl_statistics =
    {
      satisfied: int;
      total: int;
      errors: int;
    }

end
include module type of TYPES

val config_section: options Sc_config.Section.section

val run
  : _ Sc_project.Types.project
  -> Sc_project.Types.testsuite
  -> eacsl_statistics option Lwt.t
