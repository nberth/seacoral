(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Helpers for compilation processes. *)

open Sc_sys.File.TYPES

(** {2 Error reporting} *)

(** Errors raised as results of commands below. *)

type error = private
  | Process_error: 'inputs file_operation * 'inputs * Unix.process_status -> error

and _ file_operation = private
  | Syntax_check_file: string file_operation
  | Compile_C_file: string file_operation
  | Compile_Cxx_file: string file_operation
  | Link_object_files: string list file_operation

(** Carrier for errors. *)
exception ERROR of error

(** Dedicated generic printer for errors. *)
val pp_error: error Fmt.t

(* --- *)

(** Log module dedicated to outputs of [clang] *)
module Log_lwt_clang: Ez_logs.T

(** Log module dedicated to outputs of [clang++] *)
module Log_lwt_clangxx: Ez_logs.T

(** [cppflags_of_header_dirs] returns include (-I) arguments for the given
    directories. *)
val cppflags_of_header_dirs: dir list -> string list

(** [ldflags_of_library_dirs] returns linker (-L) arguments for the given
    directories. *)
val ldflags_of_library_dirs: dir list -> string list

(** [ldflags_of_library_names] returns linker (-l) arguments for the given
    library names. *)
val ldflags_of_library_names: string list -> string list

(** Prints the format prefixed with [Config.Env.clang_exe]. *)
val clang: ('a, string) Basics.PPrt.func

(** Prints the given format prefixed with [Config.Env.clangxx_exe]. *)
val clangxx: ('a, string) Basics.PPrt.func

val exec_file_name: _ file -> string

val dll_file_name: _ file -> string

(** {2 Specialized commands}

    In commands below:

    - [clang_cmd] is the name of the [clang] executable. It defaults to the
      value of {!Env.clang_exe}, {i i.e} [$CLANG] or else "clang";

    - [clangxx_cmd] is the name of the [clang++] executable. It defaults to the
      value of {!Env.clangxx_exe}, {i i.e} [$CLANGXX] or else "clang++";

    - [ld_cmd] is the name of the linker executable. It defaults to the value of
      {!Env.clang_exe}, {i i.e} [$CLANG] or else "clang";

    - [cppflags] is a list of strings passed as arguments to the C per-processor
      (empty by default).  {e Note the value of [$CPPFLAGS] ({!Env.cppflags}) is
      always appended};

    - [cflags] is a list of strings passed as arguments to the C compiler
      (empty by default — not to be);

    - [ldflags] is a list of strings passed as arguments to the linker (empty by
      default).  {e Note the value of [$LDFLAGS] ({!Env.ldflags}) is always
      appended, {b except when LLVM bitcode is to be produced}};

    - [stdout_grabber] and/or [stderr_grabber] may also be used to grab streams
      of lines output by the underlying compilation processes.  This happens in
      addition to their inclusion in logs at debug level.

    These commands may raise {!ERROR}.
*)

(** [clang_check source_file] uses [clang] to perform a syntax check on the
    given source file.  Please refer to {!clang_c} for semantics of named
    arguments. *)
val clang_check
  : ?clang_cmd:string
  -> ?cflags:string list
  -> ?cppflags:string list
  -> ?stdout_grabber: Sc_sys.Process.stream_grabber
  -> ?stderr_grabber: Sc_sys.Process.stream_grabber
  -> [< `C | `CXX] file
  -> unit Lwt.t

(** [clang_c ?clang_cmd ?cflags ?cppflags ?osuff file] starts the compilation of
    a given C file [file], and returns a promise for the corresponding object
    file.

    Starts the compilation of a C file and return the object file in an Lwt
    promise. If the compilation fails raises [Failure].
    Param [clang_cmd]: the name of the clang executable (default:
    Config.Env.clang_exe).
    Param [cflags]: the compilation flags (default: ["-g"]).
    Parem o_suff: a string added between the file name and the ".o" file
    extension (clang_c ~osuff:"foo" "file.c" -> "filefoo.o") *)
val clang_c
  : ?clang_cmd:string
  -> ?cflags:string list
  -> ?cppflags:string list
  -> ?o_dir:dir
  -> ?o_suff:string
  -> ?stdout_grabber: Sc_sys.Process.stream_grabber
  -> ?stderr_grabber: Sc_sys.Process.stream_grabber
  -> [> `C] file
  -> [`O] file Lwt.t

(** Same as clang_c, but for C++ files. *)
val clang_cxx
  : ?clangxx_cmd:string
  -> ?cxxflags:string list
  -> ?cppflags:string list
  -> ?o_dir:dir
  -> ?o_suff:string
  -> ?stdout_grabber: Sc_sys.Process.stream_grabber
  -> ?stderr_grabber: Sc_sys.Process.stream_grabber
  -> [> `CXX] file
  -> [`O] file Lwt.t

(** Starts the compilation of the file with LLVM. *)
val clang_llvm
  : ?cppflags:string list
  -> ?ldflags:string list
  -> ?stdout_grabber: Sc_sys.Process.stream_grabber
  -> ?stderr_grabber: Sc_sys.Process.stream_grabber
  -> [> `C] file
  -> [`BC] file Lwt.t

(** Starts a linking process.  [output_filename] is a function that returns a
    file name for a produced executable or library, based on the name of the
    given file. *)
val clang_ld
  : ?ld_cmd:string
  -> ?output_filename: ('a file -> string)
  -> ?ldflags:string list
  -> ?o_files:[> `O] file list
  -> ?stdout_grabber: Sc_sys.Process.stream_grabber
  -> ?stderr_grabber: Sc_sys.Process.stream_grabber
  -> ([< `C | `O] as 'a) file
  -> [< `exe | `so] file Lwt.t

val ld
  : output_file: 'x file
  -> ?ldflags:string list
  -> ?log_command:Sc_sys.Process.command_log_conditions
  -> [> `O] file list
  -> ([< `exe | `so | `O] as 'x) file Lwt.t

(** Invokes `objcopy` to redefine symbols in a `.o` file. *)
val redefine_sym
  : old:string
  -> new_:string
  -> ?log_command:Sc_sys.Process.command_log_conditions
  -> [> `O] Sc_sys.File.t
  -> unit Lwt.t
