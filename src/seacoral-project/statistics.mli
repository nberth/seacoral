(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** A module managing tool statistics. They are stored in '.stat' files with a
    JSON format for each run. *)

(** The internal type of statistics. *)
type t
and tool_stats =
  {
    time: float;
    status: (unit, string) Result.t;                 (* TODO: anything better *)
    tests_generated: int;
  }

(** Creates a new stats value in [dir]. The file name is built from the
    [run_num]ber. *)
val make: dir:Sc_sys.File.dir -> run_num:int -> t

(** Registers the statistics for a given tool. If statistics already existed for
    the tool, it is erased and replaced by the new value. *)
val report: stats:t -> tool:string -> tool_stats -> unit

(** Writes the registered statistics into the file. *)
val save: t -> unit

(** Reads all the statistics finalized up to now. *)
val load_from: dir:Sc_sys.File.dir -> t list Lwt.t

(** Prints the current statistics. *)
val print: Format.formatter -> t -> unit
