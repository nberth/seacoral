(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** [new_in ~workdir ~loaded_config_file] initializes working directory
    [workdir], dumping relevant runtime configuration along the
    way. [loaded_config_file] should be {i the} TOML file (ony one for now) that
    has been loaded to setup [workdir]. *)
val new_in
  : workdir: Sc_sys.File.dir
  -> loaded_config_file: Sc_config.Eztoml.file option
  -> Types.run Lwt.t

(** [successive_configs run] returns a stream of TOML tables with actual run
    configuration for each successive run before and including [run]. *)
val successive_configs
  : Types.run
  -> Toml.Types.table Lwt_stream.t

val ref_time
  : Types.run
  -> float

val ref_time_of
  : run_num: int
  -> Types.run
  -> float
