(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** {1 Lwt-specific routines for handling files.} *)

val descriptor
  : 'a File.t
  -> Unix.open_flag list
  -> Unix.file_perm
  -> Lwt_unix.file_descr Lwt.t

val exists: _ File.t -> bool Lwt.t
val misses: _ File.t -> bool Lwt.t
val stat: _ File.t -> Unix.stats Lwt.t
val link: ?dry: bool -> 'a File.t -> 'b File.t -> unit Lwt.t
val copy_in: dir:File.dir -> 'a File.t -> 'a File.t Lwt.t
val move_in: dir:File.dir -> _ File.t -> unit Lwt.t

val empty: _ File.t -> unit Lwt.t
val touch: _ File.t -> unit Lwt.t

val unlink: _ File.t -> unit Lwt.t

(** Tells whether [f2] is missing or it has a modification time older than
    [this] *)
val newer_than: this:_ File.t -> _ File.t -> bool Lwt.t

(** [mkdir dir]
    Applies the command `mkdir -p dir` iff `dir` does not contain
    any of the following characters: ' ', '&', ' ', '|', ';'.
    Raises `Exceptions.Invalid_dir_name dir` otherwise *)
val mkdir: string -> File.dir Lwt.t
val touch_dir: File.dir -> unit Lwt.t

(** Returns [true] if the directory in argument is empty. *)
val dir_is_empty: File.dir -> bool Lwt.t

(** [files_of_dir dir] streams the names of every regular file (i.e, not
    directories) in [dir]. *)
val files_of_dir: File.dir -> 'a File.t Lwt_stream.t

(** [dirs_of_dir dir] streams the names of every sub-directory directly within
    [dir]. *)
val dirs_of_dir: File.dir -> 'a File.t Lwt_stream.t

(** [unlink_files_of_dir dir] unlinks (i.e, removes) every regular file within
   [dir]. *)
val unlink_files_of_dir: File.dir -> unit Lwt.t

(** {2 Directory-wide locks *)

type dirlock_operation = Read | ReadWrite

(** [with_lock_in dir op f] calls the promise [f] after having acquired a lock
    in directory [dir] (using a file `.lock` to hold locks).  The lock is
    released whenever [f] is fulfilled or rejected.  [op] indicates whether the
    lock is aqcuired only for reading files in [dir] or both reading and
    writing.  *)
val with_lock_in: File.dir -> dirlock_operation -> (unit -> 'a Lwt.t) -> 'a Lwt.t

(** {2 Management of symbolic links} *)

(** [resymlink ~to_dir ~src ~dst] creates or replaces a symbolic link [dst] to
   [src].  See {!Unix.symlink} for restrictions, and semantics of [to_dir]. *)
val resymlink: to_dir:bool -> src:string -> dst:string -> unit Lwt.t

(** [newsymlink ~to_dir ~src ~dstformat] creates a new symbolic link to [src].
   The name of the link is returned; it is formed using [dstformat] with a
   sequence of increasing strictly positive Integers, and selecting the first
   name available for a symbolic link. *)
val newsymlink: to_dir:bool -> src:string -> dstformat:(int -> string)
  -> string Lwt.t

(** [redirect_symlinks_of ~dir ~src ~dst] updates all symbolic directories of
    [dir] that point to the entry basename [src] (within [dir]) so that it
    points to the entry whose basename is [dst] (within [dir] as well). *)
val redirect_symlinks_of: dir: File.dir -> src: string -> dst: File.dir -> unit Lwt.t

(** {2 String-based manipulations} *)

val read: _ File.t -> string Lwt.t
val write: _ File.t -> string -> unit Lwt.t
val digest: _ File.t -> Digest.t Lwt.t

(** {2 Syntax extensions} *)

(** Let-bindings for easy access to lwt I/O channels.  See {!Lwt_io} for
    operations available on such channels. *)
module Syntax: sig

  (** [let<* ic = file in e] binds [ic] to an LWT input channel that reads from
      [file] in the context of [e].  [ic] is closed whenever [e] is fulfilled or
      rejected.  May raise {!Unix.Unix_error}. *)
  val (let<*): _ File.t -> (Lwt_io.input_channel -> 'a Lwt.t) -> 'a Lwt.t

  (** [let>* oc = file in e] binds [oc] to an LWT output channel that writes to
      a {e new or emptied} [file] in the context of [e].  [oc] is closed
      whenever [e] is fulfilled or rejected.  May raise {!Unix.Unix_error}. *)
  val (let>*): _ File.t -> (Lwt_io.output_channel -> 'a Lwt.t) -> 'a Lwt.t

  (** Equivalent to [let>*], but with a pretty-printing formatter. *)
  val (let>*%): _ File.t -> (Lwt_fmt.formatter -> 'a Lwt.t) -> 'a Lwt.t

  (** Equivalent to [let>*%], but with a non-LWT pretty-printing formatter. *)
  val (let>*%!): _ File.t -> (Format.formatter -> 'a Lwt.t) -> 'a Lwt.t

  (** [let>>* oc = file in e] binds [oc] to an LWT output channel that writes to
      the end of [file] in the context of [e] ([file] is created if it does not
      exist).  [oc] is closed whenever [e] is fulfilled or rejected.  May raise
      {!Unix.Unix_error}. *)
  val (let>>*): _ File.t -> (Lwt_io.output_channel -> 'a Lwt.t) -> 'a Lwt.t

  (** Equivalent to [let>>*], but with a pretty-printing formatter. *)
  val (let>>*%): _ File.t -> (Lwt_fmt.formatter -> 'a Lwt.t) -> 'a Lwt.t

  (** Equivalent to [let>>*%], but with a non-LWT pretty-printing formatter. *)
  val (let>>*%!): _ File.t -> (Format.formatter -> 'a Lwt.t) -> 'a Lwt.t

end
