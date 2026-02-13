(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Type definitions for workspaces *)

(** Representation of generic workspaces: gathers a directory part and an
    environment. *)
type workspace =
  {
    workdir: Sc_sys.File.dir;            (** Working directory for a tool *)
    resroot: Resource.resdir_root;       (** Path to root resources directory *)
  }

(** Options that impact the behavior of tools during initialization phase. *)
type initialization_options =
  {
    force_preprocess: bool;
  }

type 'a generic_preprocessing_error =
  {
    workdir_status: [`Created | `Reused];
    error: 'a;
  }

(** {2 Access for sub-processes} *)

type compiled_subprocess_info =
  {
    env: string array;                  (** bindings to environment variables *)
    libs: string list;                  (** dlls required, if any *)
    cppflags: string list;              (** C header files, if any *)
  }
