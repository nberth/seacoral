(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Generic helpers for fuzzing tools *)

open Basics
open Sc_sys.File.TYPES
open Sc_project.Types

open Lwt.Syntax
open Sc_sys.File.Syntax
open Sc_sys.Lwt_file.Syntax

let install_resources_in: workspace:_ -> dir Lwt.t =
  Sc_core.Workspace.install_resources_in @@
  Sc_core.Resource.register_crunched "fuzz" (module Resources)

let emit_assignment_to_globals ppf (struct_name, Sc_C.Types.{ glob_vars }) =
  PPrt.pp_lst ~fopen:"@[<h>" ~fsep:";@\n" ~fclose:";@]@\n" ~fempty:""
    (fun ppf (t, v, _) ->
       Sc_C.Printer.pp_typed_assignment ppf (t, v, Fmt.str "%s.%s" struct_name v))
    ppf glob_vars

let emit_effective_inputs ppf (test_struct_name, func_args) =
  PPrt.pp_lst ~fopen:"@[<hv>" ~fsep:",@\n" ~fclose:"@]"
    (fun ppf v -> Fmt.pf ppf "%s.%s" test_struct_name (Sc_C.Defs.var_name v))
    ppf func_args

let emit_oracle_assessment_macro ppf =
  Fmt.pf ppf "#define __sc_assess_oracle(e)           \\\
              @\n  do {                                  \\\
              @\n    if (!(e)) /* do nothing actually */ \\\
              @\n      __asm__ volatile (\"\");            \\\
              @\n } while (0)"

let emit_llvmfuzzer_harness ppf project =
  let test_struct = project.params.test_struct
  and func = project.params.func_repr in
  Fmt.pf ppf
    "extern \"C\" int LLVMFuzzerTestOneInput (const unsigned char *__data,\
     @\n                                         size_t __size) {\
     @\n  __SC_FUZZ_LABELSONLY_REINIT ();\
     @\n  %a * __copy = __sc_raw_decode (__data, __size);\
     @\n  if (__copy != NULL) {\
     @\n    __SC_FUZZ_CATCH_ASSUME_ZERO ({\
     @\n      @[<v>%a%t@]\
     @\n      __SC_FUZZ_COMMIT ();\
     @\n    });\
     @\n  }\
     @\n  __SC_FUZZ_LABELSONLY_FILTER ();\
     @\n  __sc_raw_decoder_reset (1);\
     @\n  return 0;\
     @\n}"
    Sc_values.Struct.print test_struct
    emit_assignment_to_globals ("(*__copy)", func.func_env)
    (Sc_project.Printer.C.emit_testcall project.params
       ~oracle_assessment:"__sc_assess_oracle"
       ~emit_effective_inputs ("(*__copy)", func.func_args))

let make_harness_cxx (module Log: Ez_logs.T) ~outdir ?dry project
  : [`CXX] file Lwt.t =
  let outfile = outdir / "harness.cxx" in
  let* () =
    if not (Sc_sys.File.exists outfile) &&
       not (Sc_sys.File.(err_if ?dry "File `%a' does not exist" print outfile))
    then begin
      Log.debug "Generating `%a'" Sc_sys.File.print outfile;
      let>*% ppf = outfile in
      Lwt_fmt.fprintf ppf
        "/* Auto-generated file */\
         @\n\
         @\nextern \"C\" {\
         @\n\
         @\n#include \"decoder.h\"\
         @\n#include %S\
         @\n%t\
         @\n\
         @\n} /* extern \"C\" */\
         @\n\
         @\n%a\
         @."
        (Sc_sys.File.absname project.func_header)
        emit_oracle_assessment_macro
        emit_llvmfuzzer_harness project
    end
    else Lwt.return ()
  in
  Lwt.return outfile
