(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** [get ?default v] retrives the value bound to [v] from the environment.
    Raises {!Failure} if [v] is unbound, unless [default] is provided, in
    which case [default] is the value returned. *)
val get: ?default:string -> string -> string

(** [get_opt] is an alias of {!Sys.getenv_opt}. *)
val get_opt: string -> string option

(** [append env1 env2] returns a new environment with definitions of both
    [env1] and [env2].  As of now behavior with duplicate definitions is
    undefined.  *)
val append: string array -> string array -> string array
val print: string array Basics.PPrt.pp
