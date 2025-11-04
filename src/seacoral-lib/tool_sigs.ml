(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Signature of tools that are registered using modules *)

(** {2 Preliminary helper types} *)

(** A project where the type of raw tests is an existantial. *)
type boxed_project =
  | P: _ Sc_project.Types.project -> boxed_project                    [@@unboxed]

(** Type of setup functions that accept boxed projects. *)
and 'a setup = Sc_core.Types.workspace -> optional: bool -> boxed_project -> 'a Lwt.t

(** {2 Various tool signatures, depending on the needs} *)

module type WITH_PARAMETRIC_PROJECT = sig

  (** Ready state for the tool. *)
  type 'raw_test ready

  (** Setup the tool so it is ready to start its job. *)
  val setup
    : Sc_core.Types.workspace
    -> optional: bool
    -> 'raw_test Sc_project.Types.project
    -> 'raw_test ready Lwt.t

  (** Start the tool's main job. *)
  val run
    : 'raw_test ready
    -> unit Lwt.t

end

module type WITH_BOXED_PROJECT = sig

  (** Ready state for the tool. *)
  type ready

  (** Setup the tool so it is ready to start its job. *)
  val setup: ready setup

  (** Start the tool's main job. *)
  val run: ready -> unit Lwt.t

end
