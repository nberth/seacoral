(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

val section : Types.config Sc_config.Section.section

(** Returns the css file content given the format in the configuration.
    Raises RESOURCE_ERROR Missing_resource_file if the CSS file is not
    present in the resource directory. *)
val css_file : Types.config -> string
