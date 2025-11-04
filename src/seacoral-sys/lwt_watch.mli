(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** [waitfor ~inot file] promises to terminate whenever the directory entry
   (file or directory) [file] exists.  Reuses the inotify descriptor [inot], if
   provided. *)
val waitfor: ?inot:Lwt_inotify.t -> _ File.t -> unit Lwt.t

(** [monitor_dir ~on_close dirname] asynchronously monitors file creations or
   modifications in directory [dirname], and calls [on_close] with the name [f]
   of any such file after it has been closed.

   This function awaits for the creation of [dirname] if it does not exist upon
   call.  Note, thought, that if such is the case then that the monitoring
   cannot start right at the time the directory is created: for this reason, one
   cannot guarantee that every event triggers a call to [on_close] in this case.
   This limitation does not apply in case [dirname] already exists upon call.

   Note: inotify events are queued whenever there is one non-terminated call to
   [on_close].

   Fails with {!File.MISSING} if [dir] is not, or does not end up being, a
   directory. *)
val monitor_dir
  : on_close:(_ File.t -> unit Lwt.t)
  -> File.dir
  -> (unit -> unit Lwt.t) Lwt.t

val monitor_file
  : on_close:('a File.t -> unit Lwt.t)
  -> 'a File.t
  -> (unit -> unit Lwt.t) Lwt.t

module ASYNC: sig
  (** Asynchronous versions, where [on_close] may be called any number of times
      without waiting for previous non-terminated calls. *)

  (** [monitor_dir ~on_close dirname] creates a thread that monitors file
      creations in directory [dirname]: [on_close] is subsequently called with any
      file [f] newly created in [dirname].

      The function promised must be called at most once to terminate the
      registration of [f].

      Please see limitations of {!monitor_dir} for the case [dirname] does
      not exist upon call. *)
  val monitor_dir
    : on_close:(_ File.t -> unit Lwt.t)
    -> File.dir
    -> (unit -> unit Lwt.t) Lwt.t
end
