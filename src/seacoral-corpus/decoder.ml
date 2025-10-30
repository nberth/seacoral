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
open Sc_core.Types
open Types

open Lwt.Syntax
open Sc_sys.File.Syntax
open Sc_sys.Lwt_file.Syntax

module Log = (val (Ez_logs.from_src @@ Logs.Src.create "Sc_corpus.Decoder"))

type 'raw_test t =
  {
    params: 'raw_test decoder_params;
    workspace: workspace;
    incdir: dir;
  }

let make ~workspace params =
  let* incdir = Sc_values.Raw_decoder.install_resources_in ~workspace in
  Lwt.return {
    params;
    workspace;
    incdir;
  }

let gen_n_compile_decoder (type r) (decoder: r t) () =
  let decoder_h = decoder.workspace.workdir / "decoder.h"
  and decoder_c = decoder.workspace.workdir / "decoder.c" in
  let { test_struct; cil; test_repr } : r decoder_params = decoder.params in
  let test_repr = (test_repr :> (module Sc_values.Struct.REPR)) in
  let* () =
    Log.debug "Generating `%a'" Sc_sys.File.print decoder_h;
    let>*% ppf = decoder_h in
    Lwt_fmt.fprintf ppf "\
        @\n/* Auto-generated file */\
        @\n\
        @\n#include <stddef.h> /* size_t, maybe others */\
        @\n#include <stdint.h> /* if needed */\
        @\n#include \"sc-raw-decoder.h\"\
        @\n\
        @\n/* Type declarations, if any */\
        @\n%a\
        @\n\
        @\n/* Whole test inputs structure */\
        @\n%a;\
        @\n\
        @\n%a\
        @\n__sc_raw_decode (const unsigned char *data, size_t size);\
        @\n\
        @\n#define __sc_raw_decoder_init() __sc_decoder_init ()\
        @\n#define __sc_raw_decoder_reset(free) __sc_decoder_reset (free)\
        @\n"
      Sc_C.Printer.pp_typ_defs cil
      Sc_values.Raw_decoder.declare_struct test_struct
      Sc_values.Raw_decoder.emit_struct_ptr_typ test_struct
  and* () =
    Log.debug "Generating `%a'" Sc_sys.File.print decoder_c;
    let>*% ppf = decoder_c in
    Lwt_fmt.fprintf ppf "\
        @\n/* Auto-generated file */\
        @\n\
        @\n#include \"decoder.h\"\
        @\n\
        @\n%a\
        @\n\
        @\n%a\
        @\n__sc_raw_decode (const unsigned char *data, size_t size) {\
        @\n  __sc_buff_t __buff = { .from = data, .remaining_bytes = (long) size };\
        @\n  return (%a) %a (&__buff, 0u);\
        @\n}\
        @\n"
      Sc_values.Raw_decoder.emit_struct_decoder (test_repr, test_struct)
      Sc_values.Raw_decoder.emit_struct_ptr_typ test_struct
      Sc_values.Raw_decoder.emit_struct_ptr_typ test_struct
      Sc_values.Raw_decoder.emit_struct_decoder_symbol test_struct
  in
  let* decoder_o =
    Sc_C.Cmd.clang_c decoder_c
      ~cflags:["-fPIC"]
      ~cppflags:["-I"; Sc_sys.File.absname @@ decoder.workspace.workdir;
                 "-I"; Sc_sys.File.absname @@ decoder.incdir;
                 "-D__SC_RAW_DECODER_AVOID_MALLOC_0"]
  in
  let decoder_so = decoder.workspace.workdir / "decoder.so" in
  Sc_C.Cmd.clang_ld decoder_o
    ~output_filename:(fun _ -> Sc_sys.File.name decoder_so)
    ~ldflags: ["-shared"; "-fPIC"]

let provide_decoder decoder () =
  Lwt.return (decoder.workspace.workdir / "decoder.so")

let gen_decoder_so decoder =
  let workdir = decoder.workspace.workdir in
  Sc_sys.Lwt_file.with_lock_in workdir ReadWrite begin fun () ->
    Sc_sys.Lwt_lazy.persist_in ~dir:workdir
      (gen_n_compile_decoder decoder)
      (provide_decoder decoder)
  end

let for_compiled_subprocess ?(ld_preload_var = "LD_PRELOAD") decoder =
  let* decoder_so = gen_decoder_so decoder in
  let abspath_to_decoder_so = Sc_sys.File.absname decoder_so in
  Lwt.return {
    env = [| Fmt.str "%s=%s" ld_preload_var abspath_to_decoder_so |] ;
    libs = [ abspath_to_decoder_so ];
    cppflags = ["-I"; Sc_sys.File.absname @@ decoder.workspace.workdir;
                "-I"; Sc_sys.File.absname @@ decoder.incdir];
  }
