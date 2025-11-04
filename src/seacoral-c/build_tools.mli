(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

type config =
  {
    clang_path: string;
    clangxx_path: string;
    ld_path: string;
    objcopy_path: string;
    cppflags: string;
    ldflags: string;
  }

val config_section: config Sc_config.Section.section

(** WARNING: configuration settings of section [build-tools] that are loaded
    AFTER this lazy value has been forced will be ignored. *)
val config: config Lazy.t
