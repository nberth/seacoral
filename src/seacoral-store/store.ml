(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Basics
open Sc_sys.Lwt_memmap.TYPES
open Sc_core.Types
open Types

open Lwt.Syntax
open Sc_sys.File.Syntax
open Sc_sys.Lwt_memmap.Syntax

(* --- *)

(* Should match definitions in `sc_store_stubs.c` *)
let map_file_envvar = "SC_STORE_MAP_FILE"

(* --- *)

module Log = (val (Ez_logs.from_src @@ Logs.Src.create "Sc_store"))

(* --- *)

let resource_installer =
  Sc_core.Resource.register_crunched "store" (module Res)

(* --- *)

(* module InputsCache = Ephemeron.K1.Make (String) *)
module InputsCache = Hashtbl.Make (Digest)

type t =
  {
    store: [`bin] Sc_sys.File.t;
    store_so: [`so] Sc_sys.File.t;
    finalize: unit -> unit Lwt.t [@warning "-unused-field"];
    bars: memory_array read_write;
    array_size: int;
    termination_signal: [`Done | `Cancel] Lwt_condition.t;
  } [@@warning "-unused-field"]                            (* for ocaml < 5.1 *)

and memory_array =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let all_treated { bars; array_size; _ } : bool Lwt.t =
  (* Starting from the larger indices may actually be beneficial in general, as
     we may assume earlier indices should be treated faster by the tools. *)
  let||>* array = bars in
  let rec loop = function
    | i when i < 0 -> Lwt.return true
    | i when Bigarray.Array1.unsafe_get array i == 0 -> Lwt.return false
    | i -> loop (i - 1)
  in
  loop (array_size - 1)

let covinfo { bars; array_size; _ } : covinfo Lwt.t =
  let||>* array = bars in
  let rec loop ((c, u) as acc) id =
    if id = 0
    then Lwt.return acc
    else if array.{id - 1} land 0x80 != 0                       (* uncoverable *)
    then loop (c, Ints.add id u) (id - 1)
    else if array.{id - 1} != 0                                     (* covered *)
    then loop (Ints.add id c, u) (id - 1)
    else loop acc (id - 1)
  in
  let* c, u = loop (Ints.empty, Ints.empty) array_size in
  Lwt.return { covered_ids = c; uncoverable_ids = u; num_ids = array_size }

(* Note [labels] must be non-empty. *)
let check_labels ~toolname { array_size; _ } (labels: Ints.t) =
  let min, max = Ints.min_elt labels, Ints.max_elt labels in
  if min < 1
  then Fmt.invalid_arg "id < 1 (id=%d --- reported by %s)" min toolname
  else if max > array_size
  then Fmt.invalid_arg "id > |proof objectives| (id=%d, |proof \
                        objectives|(=array_size)=%d --- reported by %s)\
                       " max array_size toolname

let share_status ~toolname ({ bars; _ } as rdb) status labels =
  if Ints.is_empty labels
  then Lwt.return ()
  else begin
    check_labels ~toolname rdb labels;
    let* old_status =
      let<||* array = bars in
      match status with
      | `Cov ->
          Lwt.return @@
          let c = Char.code 'c' in (* TODO: get tool-specific char (from
                                      config?) *)
          Ints.fold begin fun id acc ->
            let prev = array.{id - 1} in
            if prev = 0
            then (array.{id - 1} <- c; acc)
            else if prev land 0x80 == 0 then acc
            else IntMap.add id (Fmt.str "%c" (Char.chr (prev land 0x7f)),
                                Sc_C.Types.Uncoverable) acc
          end labels IntMap.empty
      | `Uncov ->
          Lwt.return @@
          Ints.fold begin fun id acc ->
            let prev = array.{id - 1} in
            if prev = 0
            then (array.{id - 1} <- 0x80; acc)    (* TODO: get tool-specific char
                                                     (from config?) *)
            else if prev land 0x80 != 0 then acc
            else IntMap.add id (Fmt.str "%c" (Char.chr prev),
                                Sc_C.Types.Covered []) acc
          end labels IntMap.empty
      | `Unk ->
          Lwt.return @@
          IntMap.empty                                          (* Do nothing *)
    in
    if IntMap.is_empty old_status
    then Lwt.return ()
    else
      let old_status =
        IntMap.map begin fun (s, v: _ * Sc_C.Types.cov_label_status) ->
          s, match v with Covered _ -> `Cov
                        | Uncoverable -> `Uncov
                        | Unknown -> `Unk
        end old_status
      in
      raise @@ Types.Proof_inconsistency { old_status; toolname;
                                           new_status = status }
  end

let start_termination_notifier ({ store; termination_signal; _ } as t) =
  (* TODO: Use a mutex for startup (be sure the async waits before
     broadcasting) *)
  let* stop_watcher =
    Sc_sys.Lwt_watch.monitor_file store
      ~on_close:begin fun _f ->
        let* all_done = all_treated t in
        if all_done
        then begin
          Log.info "Every objective has been satisfied, shutting processes down.";
          Lwt_condition.broadcast termination_signal `Done
        end;
        Lwt.return ()
      end
  in
  Lwt.async begin fun () ->
    let* _ = Lwt_condition.wait termination_signal in
    stop_watcher ()
  end;
  Lwt.return ()

let build_store_so ~workspace =
  let* resdir =
    Sc_core.Workspace.install_resources_in ~workspace resource_installer
  in
  let store_c = resdir / "store.c"
  and store_so = workspace.workdir / "store.so" in
  let* store_ok = Sc_sys.Lwt_file.newer_than ~this:store_c store_so in
  if store_ok
  then begin
    Log.debug "%s is up to date: not recompiling." (Sc_sys.File.name store_so);
    Lwt.return store_so
  end else begin
    (* Note: Directory-wide locking needs to be put in place if `store.so` is to
       be placed within the resource directory `resdir`.  This involves using a
       read&write lock on `resdir` during the call to `clang`, and either: (i)
       acquiring a read lock on the same directory whenever a tool may use that
       library; or (ii) linking/copying the resulting `store.so` into `workdir`
       (with the caveat that a symbolic link would not work, and that `resdir`
       may not be on the same filesystem as `workdir` — which would mean that a
       standard link wouldn't work either).

       For now we only need to read-lock to prevent other processes from messing
       up `store.c` while it is being compiled; although with the out-of-date +
       hash check in place this is very unlikely. *)
    Sc_sys.Lwt_file.with_lock_in resdir Read begin fun () ->
      Sc_C.Cmd.clang_ld store_c
        ~output_filename:(fun _ -> Sc_sys.File.name store_so)
        ~ldflags: ["-shared"; "-fPIC"]
    end
  end

let make ~workspace Types.{ num_labels; inhibit_auto_termination } =
  let store = workspace.workdir / "store" in
  let* store_so = build_store_so ~workspace in
  let* bars, finalize =
    Sc_sys.Lwt_memmap.bind ~file:store ~access:ReadWrite
      ~cell_kind:Bigarray.int8_unsigned ~size:num_labels
  in
  let res =
    {
      store;
      store_so;
      bars;
      finalize;
      array_size = num_labels;
      termination_signal = Lwt_condition.create ();
    }
  in
  let* () =
    if inhibit_auto_termination
    then Lwt.return ()
    else start_termination_notifier res
  in
  Lwt.return res

let on_termination { termination_signal; _ } ~h =
  let cancel = Lwt_mvar.create_empty () in
  Lwt.async begin fun () ->
    let* s = Lwt.pick [Lwt_condition.wait termination_signal;
                       Lwt_mvar.take cancel]
    in
    match s with
    | `Done as s -> h s
    | `Cancel -> Lwt.return ()
  end;
  Lwt.return begin fun () ->
    Lwt_mvar.put cancel `Cancel
  end

let for_compiled_subprocess ?(ld_preload_var = "LD_PRELOAD")
    ({ store; store_so; _ }) =
  let abspath_to_store_so = Sc_sys.File.absname store_so in
  Lwt.return {
    env = [|
      Format.asprintf "%s=%s" map_file_envvar (Sc_sys.File.absname store);
      Format.asprintf "%s=%s" ld_preload_var abspath_to_store_so;
    |] ;
    libs = [
      abspath_to_store_so;
    ];
    cppflags = [];
  }
