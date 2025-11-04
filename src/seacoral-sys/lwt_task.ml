(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Infix

let async_start_sync_stop f =
  let count = ref 0 in
  let wait_cond = Lwt_condition.create () in
  let do_one arg =
    incr count;
    Lwt.async begin fun () ->
      Lwt.finalize begin fun () ->
        f arg
      end begin fun () ->
        decr count;
        Lwt_condition.signal wait_cond ();
        Lwt.return ()
      end
    end;
    Lwt.return ()
  and wait_all () =
    let rec aux () =
      if !count > 0
      then Lwt_condition.wait wait_cond >>= aux
      else Lwt.return ()
    in
    aux ()
  in
  do_one, wait_all

type 'res controlled_launcher = (unit -> 'res Lwt.t) -> 'res Lwt.t

let with_controlled_parallelism ~max_concurrency =
  let max_concurrency =
    if max_concurrency = 0 then max_int else max 1 max_concurrency
  in
  let pool = Lwt_pool.create max_concurrency Lwt.return in
  fun f ->
    Lwt_pool.use pool f
