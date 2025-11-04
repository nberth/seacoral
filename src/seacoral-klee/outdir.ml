(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Sc_core.Types
open Sc_sys.File.TYPES

open Lwt.Syntax

(* 1. Utils for reading the output of klee *)

let klee_dir_prefix = "klee-out"

let klee_dir_in ~workspace (tested_fun: string) (id: int) =
  (* Note: each work directory is dediacted to a single entry-point, so using
     `tested_fun` is a bit unnecessary in that regards.  Still, we keep it in case
     `klee` is launched manually in the work directory. *)
  Sc_sys.File.PRETTY.assume_in ~dir:workspace.workdir "%s.%s.%i" klee_dir_prefix
    tested_fun id

(* Returns the id of the last output directory generated and used by klee for the
   function in argument.
   Klee output file is "klee-out_tested_fun_id" *)
let lastdir ~workspace tested_fun : int option Lwt.t =
  let dirs = Sc_sys.Lwt_file.dirs_of_dir workspace.workdir in
  Lwt_stream.fold begin fun dir res ->
    if Sc_sys.File.is_dir dir
    then match String.split_on_char '.' (Sc_sys.File.basename dir) with
      (* Klee output file is "klee-out.tested_fun.id" *)
      | [kprefix; fname; id]
        when kprefix = klee_dir_prefix && fname = tested_fun ->
          (match int_of_string_opt id, res with
           | None, _ -> res
           | Some id, None -> Some id
           | Some id, Some old_id -> Some (max id old_id))
      | _ -> res
    else res
  end dirs None

let fresh ~workspace ~tested_fun : dir Lwt.t =
  let* last_id = lastdir ~workspace tested_fun in
  let id = match last_id with
    | None -> 0
    | Some i -> i + 1
  in
  Lwt.return @@ klee_dir_in ~workspace tested_fun id
