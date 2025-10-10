(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

val logs_section: Types.logs_config Sc_config.Section.section

val default: Types.options

val term
  : config_sections_that_show_up_as_arguments:
      (Sc_config.Section.any_section * [ `with_section_name_prefix
                                       | `without_section_name_prefix ]) list
  -> Types.options Cmdliner.Term.t
