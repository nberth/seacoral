(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Types

open Lwt.Syntax

module Log = (val (Ez_logs.from_src @@ Logs.Src.create "Sc_corpus.Sharing"))

let import_error ppf (test_id, exn) =
  Fmt.pf ppf "Unable@ to@ import@ test@ %s:@ @[%a@]"
    (Digest.to_hex test_id)
    Fmt.exn_backtrace (exn, Printexc.get_raw_backtrace ())

let import_tests ?(import_suff = ".imported") ?(write_test = `Link)
    ?(filter = fun _metadata -> true) corpus indir =
  let inputs = Main.existing_tests corpus in
  Lwt_stream.fold_s begin fun ({ metadata = { id; _ }; _ } as v) acc ->
    if not (filter v.metadata)
    then Lwt.return acc
    else
      let id_hex = Digest.to_hex id in
      let f = Sc_sys.File.PRETTY.not_in ~dir:indir "%s%s" id_hex import_suff in
      Lwt.catch begin fun () ->
        let* () = match write_test with
          | `Link -> v.link f
          | `Func write -> write f (Lazy.force v.raw)
        in
        Lwt.return (Basics.Digests.add id acc)
      end begin fun e ->  (* internal: simply log any error detail and proceeed. *)
        Log.err "%a" (Fmt.styled `Red import_error) (id, e);
        Lwt.return acc
      end
  end inputs Basics.Digests.empty
