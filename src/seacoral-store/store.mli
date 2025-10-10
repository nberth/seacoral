(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

type t

(** [make ~workspace params] creates a store of coverage label status. *)
val make
  : workspace: Sc_core.Types.workspace
  -> Types.params
  -> t Lwt.t

(** [share_status ~toolname store status label_ids] shares a set of [status]
    facts for the set of labels identified in [label_ids].  Those are registered
    as reported by the tool named [toolname].  May raise {!Invalid_argument} if
    an identifier in [label_ids] does not correspond to a valid label, or
    {!Sc_store.Types.Proof_inconsistency} if at least one new status contradicts
    already known results.  *)
val share_status
  : toolname:string
  -> t
  -> [`Cov | `Uncov | `Unk]
  -> Basics.Ints.t
  -> unit Lwt.t

(** [on_termination store ~h] registers a handler [h] that is called with
    argument [`Done] when all the objectives have been satisfied, or else with
    [`Stop] when the whole process stops.  [on_termination] promises (returns) a
    function that can be used to cancel the registration. *)
val on_termination
  : t
  -> h:([>`Done | `Stop] -> unit Lwt.t)
  -> (unit -> unit Lwt.t) Lwt.t

(** [for_compiled_subprocess store] promises runtime information required for
    spanwed processes to access the store. *)
val for_compiled_subprocess
  : ?ld_preload_var: string
  -> t
  -> Sc_core.Types.compiled_subprocess_info Lwt.t

(** [covinfo store] returns some statistics about coverage as measured via the
    store. *)
val covinfo: t -> Types.covinfo Lwt.t
