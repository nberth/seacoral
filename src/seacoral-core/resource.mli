(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Resource-handling utilities *)

open Sc_sys.File.TYPES

type installer_handle
type resdir_root = private dir

(** Exception raised when {!register_installer} and {!register_crunched} are
    called multiple times with the same resource name. *)
exception DUPLICATE_RESOURCE_NAME of string

(** [root dir] creates a root resource directory hierarchy.  The given directory
    may not exist yet. *)
val root: dir -> resdir_root

(** [register_installer resname installer] registers an installer promise
    [installer] under the name [resname].  The resulting handle can be passed to
    {!install} (see below) to actually call [installer] and get the associated
    installation directory. *)
val register_installer: string -> (dir -> unit Lwt.t) -> installer_handle

(** [register_crunched resname crunched] registers resources to be un-crunched
    upon installation.  This function actually makes use of
    {!register_installer}. *)
val register_crunched: string -> (module Sc_crunch.Crunched) -> installer_handle

(** {2 Procedures that actually install resources} *)

(** [install handle resdir_root] ensures the installer associated with [handle]
    has been called with a directory [dir] under [resdir_root], and returns
    [dir].

    No assumption can be made {i w.r.t} the name of [dir], except that it is
    guaranteed to be a sub-directory of [resdir_root]. *)
val install: installer_handle -> resdir_root -> dir Lwt.t

(** [install_all_in resdir_root] runs every installer registered with
    directories under [resdir_root] (if they have not already ran or started
    running within the same directory), and returns once they have all
    completed.

    This procedure may be useful to pre-install every known resources in
    advance. *)
val install_all_in: resdir_root -> unit Lwt.t
