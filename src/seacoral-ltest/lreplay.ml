(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Sc_sys.File.TYPES

open Lwt.Syntax
open Sc_sys.File.Syntax
open Sc_sys.Lwt_file.Syntax

module Log =
  (val (Ez_logs.from_src @@
        Logs.Src.create ~doc:"Logs of Lreplay" "Sc_ltest.Lreplay"))

let driver_implementation_file =
  "lreplay_driver_impl.c"

let lreplay_installed () =
  Sc_sys.Process.exec_status
    Sc_sys.Ezcmd.Std.(make "lreplay" |> key "help" |> to_cmd)

module type S = sig
  val lreplay_current_tests
    : covdir: dir
    -> ?extra_testsuite_headers: [`h] file list
    -> [> `C | `labelized] file
    -> unit Lwt.t
end

module Log_lreplay = (val Ez_logs.subproc "lreplay")

(** To circumvent the fact that we cannot give to lreplay C preprocessor flags
    that are specific to drivers, we use small proxy C files to include the
    sufficient headers before each actualy testsuite C file. *)
let proxy_testsuite_files_from ~dir ~workdir ~extra_testsuite_headers =
  Sc_sys.Lwt_file.files_of_dir dir |>
  Lwt_stream.iter_p begin fun f ->
    let>*% ppf = workdir / Sc_sys.File.basename f in
    Lwt_list.iter_s begin fun f ->
      Lwt_fmt.fprintf ppf "#include %S@\n" (Sc_sys.File.absname f)
    end (extra_testsuite_headers @ [f])
  end

module Make (Conf: Types.CONFIG) : S = struct

  let lreplay_current_tests ~covdir ?(extra_testsuite_headers = []) labelized_file =
    let* resdir =
      Sc_core.Resource.install Common.resource_installer Conf.workspace.resroot
    in
    let workdir = Conf.workspace.workdir in
    let* () = Sc_sys.Lwt_file.touch_dir workdir in
    let* () =
      proxy_testsuite_files_from ~dir:covdir ~workdir ~extra_testsuite_headers
    in
    (* The command for calling lreplay *)
    (* Checking that the test file has a main function.
       If not, the command must specify the called function. *)
    let cmd =
      [|
        "lreplay";
        "-update"; Sc_sys.File.absname labelized_file;
        "-drivers"; Fmt.str "%s/*.c" (Sc_sys.File.absname workdir);
        "-compil-files";
        Fmt.str "'%s' '%s'"
          (Sc_sys.File.absname labelized_file)
          (Sc_sys.File.absname (resdir / driver_implementation_file));
        "-other-options";
        Fmt.str "@[<h>%t@]" begin fun ppf ->
          Fmt.(list ~sep:(any " ") string) ppf
            (Sc_C.Cmd.cppflags_of_header_dirs Conf.config.header_dirs @
             Sc_C.Cmd.ldflags_of_library_names Conf.config.external_libs)
            (* TODO: probably need a `Conf.config.library_dirs` *)
        end;
      |]
    in
    Sc_sys.Process.get_promise @@
    Sc_sys.Process.exec cmd
      ~cwd:(Sc_sys.File.absname Conf.workspace.workdir)
      ~stdout:(`Log Log_lreplay.LWT.debug)
      ~stderr:(`Log Log_lreplay.LWT.debug)
      ~on_success:Lwt.return
      ~on_error:begin fun s ->
        Log.LWT.err
          "Lreplay failed on file %a with error %a.@;Command: %a"
          Sc_sys.File.print labelized_file
          Sc_sys.Process.pp_unix_status s
          Sc_sys.Process.pp_command cmd
      end
end
