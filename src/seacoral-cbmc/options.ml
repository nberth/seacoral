(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Types.OPTIONS

let default_opt = {
  mode = Cover;
  timeout = 0.;
  unwind = 10;
}

let mode_of_string s =
  match String.lowercase_ascii s with
  | "cover" -> Cover
  | "assert" -> Assert
  | "clabel" -> CLabel
  | s -> Fmt.failwith "Value %S is invalid for CBMC mode." s

let string_of_mode = function
  | Cover -> "cover"
  | Assert -> "assert"
  | CLabel -> "clabel"

let toml_entries =
  Sc_config.Eztoml.[
    float
      ~key:"timeout"
      ~doc:"Sets the timeout of the tool (default: %a). If set to 0, no timeout \
            is imposed on the CBMC process."
      ~default:default_opt.timeout
      ~runtime:true
      (fun c timeout -> {c with timeout})
      (fun c -> c.timeout);
    int
      ~key:"unwind"
      ~doc:"Sets the loop unwinding (default: %a). If set to 0, there will not \
            be unwinding."
      ~default:default_opt.unwind
      ~runtime:true
      (fun c unwind -> {c with unwind})
      (fun c -> c.unwind);
    string
      ~key:"mode"
      ~doc:"Sets CBMC's mode. It can either be \"cover\" (standard coverage \
            analysis), \"assert\" (uncoverability detection with assertions) \
            or \"clabel\" (uncoverability detection with C labels). \
            (default: %a)."
      ~env:"CBMC_MODE"
      ~default:"cover"
      ~runtime:true
      (fun c s -> {c with mode = mode_of_string s})
      (fun c -> string_of_mode c.mode);
  ]
