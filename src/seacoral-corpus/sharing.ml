(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Types

open Lwt.Syntax
open Lwt.Infix
open Sc_sys.File.Syntax
open Sc_sys.Lwt_lazy.Syntax

module Log = (val (Ez_logs.from_src @@ Logs.Src.create "Sc_corpus.Sharing"))

module type CACHE = sig
  type elt
  type t
  val empty: t
  val add: elt -> t -> t
  val mem: elt -> t -> bool
end

(** Structure returned by {!setup_sharing_manager}. *)
type 't sharing_manager =
  {
    on_share: 't -> unit Lwt.t;
    filter: 't -> bool Lwt.t;
  }

(** [setup_sharing_manager (module Set)] setups and returns a shared memory
    manager [m] of type [Set.elt sharing_mem].  [m.filter e] returns [true]
    whenever [e] was {i not} previously given to [m.on_share]. *)
let setup_sharing_manager (type t) (module C: CACHE with type elt = t) =
  let mutex = Lwt_mutex.create ()
  and shared = ref C.empty in
  let on_share id =
    Lwt_mutex.with_lock mutex begin fun () ->
      shared := C.add id !shared;
      Lwt.return ()
    end
  and filter id =
    Lwt_mutex.with_lock mutex begin fun () ->
      Lwt.return (not (C.mem id !shared))
    end
  in
  { on_share; filter }

let import_error ppf (test_id, exn) =
  Fmt.pf ppf "Unable@ to@ import@ test@ %s:@ @[%a@]"
    (Digest.to_hex test_id)
    Fmt.exn_backtrace (exn, Printexc.get_raw_backtrace ())

let with_bidirectional_channel
    ?(import_suff = ".imported") ~read_test ?(write_test = `Link)
    ?(on_import = fun _ -> Lwt.return_unit)
    ~toolname corpus indir f =
  let setup_uplink { on_share; _ } =
    Sc_sys.Lwt_watch.ASYNC.monitor_dir indir ~on_close:begin fun f ->
      if Sc_sys.File.check_suffix f import_suff
      then Lwt.return ()
      else read_test f >>= function
        | None ->
            Lwt.return ()
        | Some (test, outcome) ->
            Main.share_test' ~toolname ~on_share ~outcome corpus test
    end
  and setup_downlink { filter; _ } =
    Main.on_new_test corpus begin fun ({ metadata = { id; _ } as metadata;
                                         _ } as v) ->
      let* ok = filter id in
      if not ok then Lwt.return () else begin
        let id_hex = Digest.to_hex id in
        let f = indir / id_hex in
        let f' = Sc_sys.File.(assume @@ name f ^ import_suff) in
        let* exists = Sc_sys.Lwt_file.(exists f ||* lazy (exists f')) in
        if exists then Lwt.return () else begin
          let* () = on_import metadata in
          Lwt.catch begin fun () ->
            match write_test with
            | `Func write -> write f' (Lazy.force v.raw)
            | `Link -> v.link f'
          end begin fun e ->
            Log.LWT.err "%a" (Fmt.styled `Red import_error) (id, e);
          end
        end
      end
    end
  in
  let sharing_mem = setup_sharing_manager (module Basics.Digests) in
  let* stop_downlink = setup_downlink sharing_mem in
  let* stop_uplink = setup_uplink sharing_mem in
  Lwt.finalize f begin fun () ->
    stop_downlink () <&> stop_uplink ()
  end

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
