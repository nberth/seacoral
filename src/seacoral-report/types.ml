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
    enable: bool;
    report_format: [`Pretty | `OnePage];
    output_dir: string;
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

let pp_config_error ppf = function
  | Invalid_config_value {key; value; possible_values} ->
     Fmt.pf ppf
       "Invalid configuration value %S for configuration key %S. \
        Possible values: %a"
       key
       value
       Basics.PPrt.Strings.pp_comma__separated  possible_values

let pp_resource_error ppf = function
  | Missing_resource_file {filename} ->
     Fmt.pf ppf
       "Resource file %S cannot be found in resources directory"
       filename

;; Printexc.register_printer begin function
  | CONFIG_ERROR ce -> Some (Fmt.str "%a" pp_config_error ce)
  | RESOURCE_ERROR re -> Some (Fmt.str "%a" pp_resource_error re)
  | _ -> None
end ;;
