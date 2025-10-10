(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** [async_start_sync_stop f] provides a pair of promises [do_one] and
    [await_all].  [do_one] is equivalent to [f]; [await_all ()] may only
    complete successfuly whenever the last non-terminated promize created using
    [do_one] terminates (successfuly or not). *)
val async_start_sync_stop
  : ('a -> unit Lwt.t)
  -> ('a -> unit Lwt.t) * (unit -> unit Lwt.t)

(** A type of tasks that may be delayed depending on current resource
    consumption. *)
type 'res controlled_launcher = (unit -> 'res Lwt.t) -> 'res Lwt.t

(** [with_controlled_parallelism ~max_concurrency] creates a task launcher that
    delays a task execution if [max_concurrency] task terminations are currently
    pending. *)
val with_controlled_parallelism
  : max_concurrency: int
  -> 'result controlled_launcher
