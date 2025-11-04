(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Resource-handling utilities *)

open Sc_sys.File.TYPES

open Lwt.Syntax
open Sc_sys.File.Syntax

(* --- *)

type installer_handle = H of string                                  [@@unboxed]
type resdir_root = dir

exception DUPLICATE_RESOURCE_NAME of string

(* --- *)

let root = Fun.id

let installers: (string, dir -> unit Lwt.t) Hashtbl.t =
  Hashtbl.create 1

let register_installer: string -> (dir -> unit Lwt.t) -> installer_handle =
  fun resname installer ->
  if Hashtbl.mem installers resname then
    raise @@ DUPLICATE_RESOURCE_NAME resname;
  Hashtbl.add installers resname installer;
  H resname

let install: installer_handle -> resdir_root -> dir Lwt.t =
  let install_once =
    Sc_sys.Lwt_lazy.do_once ~keep_idents:true (module struct
      type t = installer_handle * resdir_root
      let equal = (=)
      let hash n = Hashtbl.hash n
    end)
  in
  fun (H resname as handle) resdir_root ->
    install_once ~ident:(handle, resdir_root) begin fun () ->
      let installer = Hashtbl.find installers resname in
      let dir = resdir_root / resname in
      Sc_sys.Lwt_file.with_lock_in dir ReadWrite begin fun () ->
        let* () = installer dir in
        Lwt.return dir
      end
    end

let install_all_in: resdir_root -> unit Lwt.t = fun resdir_root ->
  Hashtbl.to_seq_keys installers |>
  Lwt_seq.of_seq |>
  Lwt_seq.iter_p begin fun resname ->
    let* _dir = install (H resname) resdir_root in
    Lwt.return ()
  end

(* --- *)

let register_crunched resname crunched_resources =
  register_installer resname begin fun dir ->
    Sc_crunch.LWT.deflate_in ~dir crunched_resources
  end
