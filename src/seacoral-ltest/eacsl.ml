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

open Lwt.Syntax

(* --- *)

module Log = (val (Ez_logs.from_src @@ Logs.Src.create "Sc_ltest.Eacsl"))

(* Log module for reporting eacsl sub-process outputs *)
module Log_eacsl = (val Ez_logs.subproc "e-acsl-gcc.sh")

module Cmd = Sc_sys.Ezcmd.Std

let command_base ?(eacsl_gcc_path = "e-acsl-gcc.sh") () : Cmd.t =
  Cmd.make eacsl_gcc_path

let exec_name = "test.exe"

let is_installed ?eacsl_gcc_path () =
  Sc_sys.Process.exec_status (command_base ?eacsl_gcc_path () |>
                              Cmd.key "h" |>
                              Cmd.to_cmd)

module type S = sig

  (** Calls e-acsl on a test and returns true if the acsl specifications are
      satisfied by the test. *)
  val call_eacsl_on_file
    : ?eacsl_gcc_path: string
    -> codebase:[`C] file list
    -> [`C] file
    -> bool Lwt.t

end

module Make (C : Types.CONFIG) = struct

  let dir_from_file: _ file -> dir =
    Memo.memoize_poly begin fun file ->
      let basename = Sc_sys.File.basename file in
      let dir = Format.sprintf "e-acsl-lreplay-%s" basename in
      Sc_sys.File.mkdir_in ~dir:C.workspace.workdir dir
    end

  let eacsl_cmd ?eacsl_gcc_path c_files exec_file : Cmd.t =
    command_base ?eacsl_gcc_path ()

    |> begin fun acc ->
      List.fold_left
        (fun acc c_file -> Cmd.raw (Sc_sys.File.absname c_file) acc)
        acc c_files
    end

    (* Compile instrumented code *)
    |> Cmd.key "c"

    (* output the generated executables *)
    |> Cmd.base "oexec-e-acsl" (Sc_sys.File.absname exec_file)

    (* additional options to the prepreprocessor *)
    |> Cmd.base "e"
      (Format.sprintf "-D__SC_AVOID_INCLUDING_MATH_H__ -I%s"
         (Sc_sys.File.absname C.workspace.workdir))

    (* additional options to framac prepreprocessor *)
    |> Cmd.base "E"
      (Format.sprintf "-D__SC_AVOID_INCLUDING_MATH_H__ -I%s"
         (Sc_sys.File.absname C.workspace.workdir))

  let start_eacsl_process file cmd =
    Sc_sys.Process.get_promise @@
    Sc_sys.Process.exec cmd
      ~cwd:(Sc_sys.File.absname C.workspace.workdir)
      ~stdout:(`Log Log_eacsl.LWT.debug)
      ~stderr:(`Log Log_eacsl.LWT.debug)
      ~on_success:(fun () -> Lwt.return ())
      ~on_error:begin fun e ->                         (* TODO: custom exception *)
        Fmt.failwith "Error while e-acsl processed file %a (%a). Abort."
          Sc_sys.File.print file
          Sc_sys.Process.pp_unix_status e
      end

  let start_eacsl_replayer exec_file =
    Sc_sys.Process.get_promise @@
    Sc_sys.Process.shell (Sc_sys.File.absname exec_file)
      ~cwd:(Sc_sys.File.absname C.workspace.workdir)
      ~stdout:(`Log Log_eacsl.LWT.debug)
      ~stderr:(`Log Log_eacsl.LWT.debug)
      ~on_success:(fun () -> Lwt.return true)
      ~on_error:(fun _e -> Lwt.return false)

  let call_eacsl_on_file ?eacsl_gcc_path ~codebase (c_file: [`C] Sc_sys.File.t) =
    let dir = dir_from_file c_file in
    let eacsl_exec = Sc_sys.File.assume_in ~dir exec_name in
    let cmd = Cmd.to_cmd @@ eacsl_cmd ?eacsl_gcc_path (c_file :: codebase) eacsl_exec in
    let* () = start_eacsl_process c_file cmd in
    let* res = start_eacsl_replayer eacsl_exec in
    Log.info
      (if res
       then "Testsuite in `%a' satisfies the ACSL specification"
       else "Testsuite in `%a' do not satisfies the ACSL specification")
      Sc_sys.File.print c_file;
    Lwt.return res
end
