(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Sc_sys.File.TYPES

(** [with_bidirectional_channel ?import_suff ?read_test ?write_test ?on_import
    ~toolname corpus dirname f] calls [f] after having setup a bi-directional
    exchange protocol with the runtime database [corpus].

    According to this protocol, each file newly created in directory [dirname]
    and whose name does not terminate with [import_suff] (set to [".imported"]
    by default), is read by using [read_test] ([Corpus.read_test corpus] by
    default), and the resulting input is exported to [corpus] in the name of
    [toolname].

    Reciprocally, every file shared with [corpus] during the execution of [f]
    and until [with_bidirectional_channel] terminates is placed into [dirname]
    by using [write_test]: the latter is [`Link] by default, which means a
    filesystem link is used; alternatively a user-defined function [write] may
    be specified with [`Func write].  Files imported in this way are not subject
    to the aforementioned export mechanism.  If given, [on_import metadata] is
    called with metadata about [i] before [i] is imported; this import is
    aborted if [on_import] is rejected.

    Please see limitations of {!Utils.monitor_dir} for the case [dirname] does
    not exist upon call; limitations pertaining to the argument [write_test]
    also apply. *)
val with_bidirectional_channel
  : ?import_suff: string
  -> read_test: ('a file -> ('raw_test * Types.test_outcome) option Lwt.t)
  -> ?write_test: [< `Func of _ file -> 'raw_test -> unit Lwt.t
                  | `Link > `Link ]
  -> ?on_import: (Types.test_metadata -> unit Lwt.t)
  -> toolname: string
  -> 'raw_test Main.corpus
  -> dir
  -> (unit -> 'a Lwt.t)
  -> 'a Lwt.t

(** [import_tests ?import_suff ?write_test ?filter corpus dirname] imports every
    input previously (given to/shared with) [corpus] into the directory
    [dirname].  Inputs are imported locally using [write] if [write_test = `Func
    write], or using a file-system link otherwise (i.e, [write_test = `Link],
    the default).

    [filter] may be used to select tests to import based on their metadata ({i
    cf} {!Types.test_metatada}).

    The directory [dirname] must exist when this function is called.  *)
val import_tests
  : ?import_suff:string
  -> ?write_test:[< `Func of _ file -> 'raw_test -> unit Lwt.t
                 | `Link > `Link ]
  -> ?filter: (Types.test_metadata -> bool)
  -> 'raw_test Main.corpus
  -> dir
  -> Basics.Digests.t Lwt.t
