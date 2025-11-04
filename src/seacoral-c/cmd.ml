(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Lwt.Syntax

let optional_string = function
  | "" -> None
  | s -> Some s

(** Environment variables for configurable commands and arguments that are used
    to process C (and C++) files. *)
module ENV = struct
  let build_tools = Build_tools.config
  let clang_exe = lazy (Lazy.force build_tools).clang_path
  let clangxx_exe = lazy (Lazy.force build_tools).clangxx_path
  let ld_exe = lazy (Lazy.force build_tools).ld_path
  let objcopy_exe = lazy (Lazy.force build_tools).objcopy_path
  let cppflags = lazy (optional_string (Lazy.force build_tools).cppflags)
  let ldflags = lazy (optional_string (Lazy.force build_tools).ldflags)
end

type _ file_operation =
  | Syntax_check_file: string file_operation
  | Compile_C_file: string file_operation
  | Compile_Cxx_file: string file_operation
  | Link_object_files: string list file_operation

type error =
  | Process_error: 'inputs file_operation * 'inputs * Unix.process_status -> error

exception ERROR of error

let pp_error ppf error =
  let pp_strs =
    Basics.PPrt.pp_lst ~fopen:"@[" ~fsep:",@ " ~fclose:"@]" Fmt.string
  in
  match error with
  | Process_error (k, f, status) ->
      let ppk: Format.formatter -> unit = fun ppf -> match k with
        | Syntax_check_file ->
            Format.fprintf ppf "syntax-checking@ %s" f
        | Compile_C_file ->
            Format.fprintf ppf "compiling@ C@ file@ %s" f
        | Compile_Cxx_file ->
            Format.fprintf ppf "compiling@ C++@ file@ %s" f
        | Link_object_files ->
            Format.fprintf ppf "linking@ object@ file%s@ %a"
              (if List.length f < 2 then "" else "s") pp_strs f
      in
      Fmt.pf ppf "Error@ while@ %t:@;compiler@ exited@ with@ status@ %a"
        ppk Sc_sys.Process.pp_unix_status status

;; Printexc.register_printer begin function
  | ERROR e ->
      Some (Basics.PPrt.to_string "%a" pp_error e)
  | _ ->
      None
end;;

let cc_error (k: 'k file_operation) (f: 'k) (status: Unix.process_status) =
  raise @@ ERROR (Process_error (k, f, status))

(* --- *)

(* Log module for reporting clang's outputs *)
module Log_lwt_clang = (val Ez_logs.subproc "clang")
module Log_lwt_clangxx = (val Ez_logs.subproc "clang++")
module Log_lwt_ld = (val Ez_logs.subproc "ld")

let stream_grab grabber logger = match grabber with
  | None -> `Log logger
  | Some grabber -> `GrabNLog (grabber, logger)

(* --- *)

let cppflags_of_header_dirs (dirs: Sc_sys.File.dir list) =
  List.map (Fmt.str "-I'%a'" Sc_sys.File.print_absname) dirs

let ldflags_of_library_dirs (dirs: Sc_sys.File.dir list) =
  List.map (Fmt.str "-I'%a'" Sc_sys.File.print_absname) dirs

let ldflags_of_library_names (libs: string list) =
  List.map (Fmt.str "-l%s") libs

let cmd exe fmt = Format.asprintf ("%s "^^fmt) exe

let clangxx fmt = cmd (Lazy.force ENV.clangxx_exe) fmt
let clang fmt = cmd (Lazy.force ENV.clang_exe) fmt

let lazy_cmd lazy_path given_path =
  match given_path with
  | Some path -> path
  | None -> Lazy.force lazy_path

(** {2 Checking C/C++ source code} *)

let clang_check
    ?clang_cmd
    ?(cflags = ["-g"])
    ?(cppflags = [])
    ?stdout_grabber ?stderr_grabber
    (file: [< `C | `CXX] Sc_sys.File.t)
  : unit Lwt.t
  =
  let file_name = Sc_sys.File.name file in
  Sc_sys.Process.PRETTY.shell_unit
    (* TODO: -fcolor-diagnostics only if at least one log reporter accepts TTY
       control characters. *)
    "%s %a%a%a-c %s -fsyntax-only -fcolor-diagnostics"
    (lazy_cmd ENV.clang_exe clang_cmd)
    Basics.PPrt.Strings.pp_space_separated_ cflags
    Basics.PPrt.Strings.pp_space_separated_ cppflags
    Fmt.(option @@ fmt "%s ") (Lazy.force ENV.cppflags)
    file_name
    ~stdout:(stream_grab stdout_grabber Log_lwt_clang.LWT.debug)
    ~stderr:(stream_grab stderr_grabber Log_lwt_clang.LWT.debug)
    ~on_error:(cc_error Syntax_check_file file_name)

(** {2 Compiling C/C++ source code} *)

let o_name ?dir ?(namesuff = "") f =
  let f = match dir with
    | None -> f
    | Some d -> Filename.concat d (Filename.basename f)
  in
  Format.asprintf "%s%s.o" (Filename.remove_extension f) namesuff

let clang_c
    ?clang_cmd
    ?(cflags = ["-g"])
    ?(cppflags = [])
    ?o_dir
    ?o_suff
    ?stdout_grabber ?stderr_grabber
    (file_c : [> `C] Sc_sys.File.t)
  : [`O] Sc_sys.File.t Lwt.t
  =
  let file_c_name = Sc_sys.File.name file_c in
  let o_dir = Option.map Sc_sys.File.name o_dir in
  let file_o_name = o_name ?dir:o_dir ?namesuff:o_suff file_c_name in
  let* () =
    Sc_sys.Process.PRETTY.shell_unit "%s %a%a%a-c %s -o %s"
      (lazy_cmd ENV.clang_exe clang_cmd)
      Basics.PPrt.Strings.pp_space_separated_ cflags
      Basics.PPrt.Strings.pp_space_separated_ cppflags
      Fmt.(option @@ fmt "%s ") (Lazy.force ENV.cppflags)
      file_c_name file_o_name
      ~stdout:(stream_grab stdout_grabber Log_lwt_clang.LWT.debug)
      ~stderr:(stream_grab stderr_grabber Log_lwt_clang.LWT.debug)
      ~on_error:(cc_error Compile_C_file file_c_name)
  in
  Lwt.return @@ Sc_sys.File.existing file_o_name

let clang_cxx
    ?clangxx_cmd
    ?(cxxflags = ["-g"])
    ?(cppflags = [])
    ?o_dir
    ?o_suff
    ?stdout_grabber ?stderr_grabber
    (file_c: [> `CXX] Sc_sys.File.t)
  : [`O] Sc_sys.File.t Lwt.t
  =
  let file_c_name = Sc_sys.File.name file_c in
  let o_dir = Option.map Sc_sys.File.name o_dir in
  let file_o_name = o_name ?dir:o_dir ?namesuff:o_suff file_c_name in
  let* () =
    Sc_sys.Process.PRETTY.shell_unit "%s %a%a%a-c %s -o %s"
      (lazy_cmd ENV.clangxx_exe clangxx_cmd)
      Basics.PPrt.Strings.pp_space_separated_ cxxflags
      Basics.PPrt.Strings.pp_space_separated_ cppflags
      Fmt.(option @@ fmt "%s ") (Lazy.force ENV.cppflags)
      file_c_name file_o_name
      ~stdout:(stream_grab stdout_grabber Log_lwt_clangxx.LWT.debug)
      ~stderr:(stream_grab stderr_grabber Log_lwt_clangxx.LWT.debug)
      ~on_error:(cc_error Compile_Cxx_file file_c_name)
  in
  Lwt.return @@ Sc_sys.File.existing file_o_name

let llvm_file_name f =
  Format.asprintf "%s.bc" (Filename.remove_extension f)

let clang_llvm
    ?(cppflags = [])
    ?(ldflags = [])
    ?stdout_grabber ?stderr_grabber
    (file_c: [> `C] Sc_sys.File.t)
  : [`BC] Sc_sys.File.t Lwt.t
  =
  let file_c_name = Sc_sys.File.name file_c in
  let file_bc_name = llvm_file_name file_c_name in
  let* () =
    Sc_sys.Process.PRETTY.shell_unit
      "%s -emit-llvm %a%a-c %s %a-o %s"
      (Lazy.force ENV.clang_exe)
      Basics.PPrt.Strings.pp_space_separated_ cppflags
      Fmt.(option @@ fmt "%s ") (Lazy.force ENV.cppflags)
      file_c_name
      Basics.PPrt.Strings.pp_space_separated_ ldflags
      file_bc_name
      ~stdout:(stream_grab stdout_grabber Log_lwt_clang.LWT.debug)
      ~stderr:(stream_grab stderr_grabber Log_lwt_clang.LWT.debug)
      ~on_error:(cc_error Compile_C_file file_c_name)
  in
  Lwt.return @@ Sc_sys.File.existing file_bc_name

(** {2 Linking} *)

let exec_file_name f =
  Format.asprintf "%s.exe" (Filename.remove_extension @@ Sc_sys.File.name f)

let dll_file_name f =
  Format.asprintf "%s.so" (Filename.remove_extension @@ Sc_sys.File.name f)

let clang_ld
    ?ld_cmd
    ?(output_filename: _ Sc_sys.File.t -> string = exec_file_name)
    ?(ldflags = [])
    ?(o_files : [> `O] Sc_sys.File.t list = [])
    ?stdout_grabber ?stderr_grabber
    (file : [< `C | `O] Sc_sys.File.t)
  : [< `exe | `so] Sc_sys.File.t Lwt.t
  =
  let file_name = Sc_sys.File.name file in
  let file_out = output_filename file in
  let file_names = file_name :: List.map Sc_sys.File.name o_files in
  let* () =
    Sc_sys.Process.PRETTY.shell_unit "%s %a%a%a-o %s"
      (lazy_cmd ENV.clang_exe ld_cmd)
      Basics.PPrt.Strings.pp_space_separated_ ldflags
      Fmt.(option @@ fmt "%s ") (Lazy.force ENV.ldflags)
      Basics.PPrt.Strings.pp_space_separated_ file_names
      file_out
      ~stdout:(stream_grab stdout_grabber Log_lwt_clang.LWT.debug)
      ~stderr:(stream_grab stderr_grabber Log_lwt_clang.LWT.debug)
      ~on_error:(cc_error Link_object_files file_names)
  in
  Lwt.return @@ Sc_sys.File.existing file_out

let ld ~output_file ?(ldflags = []) ?log_command o_files =
  let o_files = List.map Sc_sys.File.name o_files in
  Sc_sys.Process.get_promise @@
  Sc_sys.Process.exec
    ~stdout:(`Log Log_lwt_ld.LWT.debug)
    ~stderr:(`Log Log_lwt_ld.LWT.debug)
    ~on_success:(fun () -> Lwt.return output_file)
    ~on_error:(cc_error Link_object_files o_files)
    ?log_command
    (Array.of_list @@
     [ Lazy.force ENV.ld_exe;
       "--relocatable";
       "--output"; Sc_sys.File.name output_file ]
     @ o_files
     @ ldflags)

let redefine_sym ~old ~new_ ?log_command (file : [> `O] Sc_sys.File.t) =
  Sc_sys.Process.get_promise @@
  Sc_sys.Process.exec
    ~on_success:Lwt.return
    ?log_command
    [|
      Lazy.force ENV.objcopy_exe;
      "--redefine-sym"; Format.sprintf "%s=%s" old new_;
      Sc_sys.File.name file;
    |]
