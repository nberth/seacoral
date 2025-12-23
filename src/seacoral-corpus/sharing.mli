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
