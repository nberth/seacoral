(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Configuration of the report builder *)
type config =
  {
    enable: bool;
    (** For enabling the report generation *)
    report_format: [`Pretty | `OnePage];
    (** The format of the report: pretty report (with tabs and nice css)
        or one page *)
    output_dir: string;
    (** The directory in which the report is generated *)
  }

type config_error =
  | Invalid_config_value of
      {
        key: string;
        value: string;
        possible_values: string list
      }

exception CONFIG_ERROR of config_error

type resource_error =
  | Missing_resource_file of
      {
        filename: string;
      }

exception RESOURCE_ERROR of resource_error
