(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** Gathers various operations needed when preprocessing C projects *)

open Sc_values.TYPES
open Sc_sys.File.TYPES
open Types

open Basics

open Lwt.Infix
open Lwt.Syntax
open Sc_sys.File.Syntax
open Sc_sys.Lwt_file.Syntax

(* --- *)

module Log = (val (Ez_logs.from_src @@ Logs.Src.create "Sc_project"))

(* --- *)

let append_file oc (file : [`C] file) =
  let* () = Lwt_io.fprintf oc "#line 1 %S\n" (Sc_sys.File.absname file) in
  let<* ic = file in
  Lwt_io.read ic >>= Lwt_io.write oc

let codebase_file_in ~dir config =
  let basename =
    match config.project_problem.input_files with
    | [c_file] -> Sc_sys.File.basename c_file       (* one file: keep its name *)
    | _c_files -> "codebase.c"
  in
  dir / basename

(* In [gather_codebase_in] below, we and prevent the codebase from ever defining
   or using symbol `main` so that symbol will always be available for the
   tools. *)

let main_replacement_symbol =
  "__sc_main"

let gather_codebase_in ~dir config : [`C] file Lwt.t =
  let file = codebase_file_in ~dir config in
  let>* oc = file in
  let* () =
    Lwt_io.fprintf oc "#define main(args...) %s(args)\n" main_replacement_symbol
  in
  let* () =
    Lwt_list.iter_s (append_file oc)
      (config.project_problem.input_files @
       config.project_problem.fixtures_files)                      (* for now *)
  in
  Lwt.return file

let patch_entrypoint_name config =
  if config.project_problem.entrypoint = "main"
  then { config with
         project_problem = { config.project_problem with
                             entrypoint = main_replacement_symbol } }
  else config

(* --- *)

let labeling_error err =
  raise @@ LABELING_ERROR err

let setup_error l =
  raise @@ SETUP_ERROR (Preproc l)

(* Checks the llvm construction of the project for a 'FunctionDecl' constructor
   followed by the entrypoint. This ensures the entrypoint is well defined. *)
let check_entrypoint_definition_in_llvm_dump_ast ~config stream =
  let rx_of_fun f = Str.regexp @@ Fmt.str ".*FunctionDecl.*%s" f in
  let ep = config.project_problem.entrypoint in
  let functions =
    match config.project_problem.annotated_functions with
    | `Only l -> List.map (fun f -> f, rx_of_fun f) (ep :: l)
    | `All | `Auto -> [ep, rx_of_fun ep]
  in
  (* Checks if [line] follows the regexp corresponding to the given function. *)
  let rec loop line rev_tested_funs = function
    | [] -> List.rev rev_tested_funs
    | (_, rx) :: tl when Str.string_match rx line 0 ->
       List.rev_append rev_tested_funs tl
    | hd :: tl -> loop line (hd :: rev_tested_funs) tl
  in
  let* unfound_functions =
    Lwt_stream.fold
      (fun l funs -> loop l [] funs)
      stream
      functions
  in
  match unfound_functions with
  | [] -> Lwt.return ()
  | l ->
     let errors =
       List.map begin fun (f, _) ->
         if f = ep
         then Missing_entrypoint f
         else Missing_cover_target f
       end l
     in
     setup_error errors

let do_syntax_check ~incdir ~config c_file =
  let stdout_lines_mbox = Lwt_mvar.create_empty () in
  let stderr_lines_mbox = Lwt_mvar.create_empty () in
  Lwt.catch begin fun () ->
    (* resources/noop-labels.h provides a dummy implementation of pc_label so as
       to syntax-check custom label conditions. *)
    let cppflags =
      "-include" :: Sc_sys.File.absname (incdir / "noop-labels.h") ::
      (Sc_C.Cmd.cppflags_of_header_dirs @@
       incdir :: config.project_problem.header_dirs)
    in
    let* () =
      Sc_C.Cmd.clang_check_and_print_llvm c_file
        ~cppflags
        ~stdout_grabber:(MBox stdout_lines_mbox)
        ~stderr_grabber:(MBox stderr_lines_mbox)
    in
    let* stream = Lwt_mvar.take stdout_lines_mbox in
    check_entrypoint_definition_in_llvm_dump_ast ~config stream
  end begin function
    | Sc_C.Cmd.ERROR cmd_error
      when !Ez_logs.stdout_level_ref <> Some Debug ->
        let* stderr_lines = Lwt_mvar.take stderr_lines_mbox in
        let* stderr_lines = Lwt_stream.to_list stderr_lines in
        labeling_error @@ Syntax_errors { cmd_error; stderr_lines }
    | Sc_C.Cmd.ERROR cmd_error ->
        labeling_error @@ Syntax_errors { cmd_error; stderr_lines = [] }
    | e ->
        Lwt.reraise e
  end

(* --- *)

(** Type names that may be seen in `typedef` definitions within user-defined
    code, and that may clash with standard headers and definitions that are used
    by Seacoral and its tools. *)
let typenames_to_substitute =
  [
    "bool";
  ]

type typename_to_substitute_state =
  | To_substitute
  | Substituted of string

class typdef_patching_visitor =
  let tbl =
    Hashtbl.of_seq @@
    Seq.map (fun t -> t, To_substitute) @@
    List.to_seq typenames_to_substitute
  in
  let visit_typedef (info: Cil.typeinfo) (t: Cil.global) : _ Cil.visitAction =
    let orig_tname = info.tname in
    match Hashtbl.find_opt tbl info.tname with
    | Some To_substitute ->
        info.tname <- "__sc_"^orig_tname;
        let post t =
          Hashtbl.replace tbl orig_tname (Substituted info.tname); t
        in
        ChangeDoChildrenPost ([t], post)
    | Some _ | None ->
        DoChildren
  in
  object
    inherit Cil.nopCilVisitor
    (* Note: we don't need to visit the use of types themselves as typeinfo
       structures (`info` below) are shared between definitions and uses of the
       type. *)
    method! vglob = function
      | GType (info, _) as t ->
          visit_typedef info t
      | _ ->
          SkipChildren
    method! vfunc _ =
      SkipChildren
    method! vinit _ _ _ =
      SkipChildren
  end

let type_patching_and_detection_visitor stddef_info_ref =
  object
    inherit typdef_patching_visitor as super
    method! vexpr = function
      | Const CInt (_, IBool, _) ->
          stddef_info_ref := { stdbool_needed = true };
          DoChildren
      | _ ->
          DoChildren
    method! vglob g =
      match super#vglob g with
      | SkipChildren ->
          DoChildren                                (* force for every global *)
      | action ->
          action
    method! vfunc _ =
      DoChildren
    method! vinit _ _ _ =
      DoChildren
  end

let patch_cil_file_types cil =
  Cil.visitCilFile (new typdef_patching_visitor) cil

let patch_and_inspect_cil_file_types cil =
  let stddef_info = ref { stdbool_needed = false } in
  Cil.visitCilFile (type_patching_and_detection_visitor stddef_info) cil;
  !stddef_info

(* --- *)

let extract_cils (c_file: [`C | `labelized] file) =
  (* Distinguish tested code from from external definitions. *)
  let dir = Sc_sys.File.dir c_file in
  let hackdefs =
    (* HACK (hopefully temporary / but proper fix may need a patch/customization
       in goblint): pre-define some C preprocessor variables so `cpp` skips the
       corresponding includes. `typedef float _Foat32` and such

       The rationale for this hack is that goblint's parser treats (some?)
       "standard" type names internally on its own, and that behavior cannot be
       prevented at the moment. For instance, `_Float32` is handled like
       something equivalent to the internal Cil type `float`, so parsing
       declarations like `typedef float _Float32`, that can be found in
       low-level header files, leads to an error in Cil.  We therefore try to
       simply avoid feeding Cil with such statements. *)
    "-D_BITS_FLOATN_COMMON_H -D_BITS_FLOATN_H"
  in
  let* full_c =
    let full_file =
      Sc_sys.File.PRETTY.assume_in ~dir "%s-with-all-headers.c" @@
      Sc_sys.File.basename ~chop_extension:true c_file
    in
    (* Create a flat fully pre-processed labelized file *)
    let* () =
      Sc_sys.Process.shell_unit @@
      Sc_C.Cmd.clang "-E %s -I. - -o '%a' < '%a'" hackdefs
        Sc_sys.File.print full_file
        Sc_sys.File.print c_file
    in
    Lwt.return full_file
  and* lib_c =
    let lib_file =
      Sc_sys.File.PRETTY.assume_in ~dir "%s-lib-headers-only.c" @@
      Sc_sys.File.basename ~chop_extension:true c_file
    in
    (* Create a flat fully pre-processed labelized file where only
       pre-processor statmenents from the source file have been kept.

       Note: non-preprocessor-related lines are blanked and not removed so we
       can rely on source locations to perform a diff of the resulting CIL
       w.r.t the CIL obtained for the full file.  This avoids issues with
       internal type names generated by {!Cil} for anonymous types. *)
    let* () =
      Sc_sys.Process.PRETTY.shell_unit
        "sed -e 's/^[^#].*$//' '%a' | %s" Sc_sys.File.print c_file @@
      Sc_C.Cmd.clang "-E %s -I. - -o '%a'" hackdefs
        Sc_sys.File.print lib_file
    in
    Lwt.return lib_file
  in
  let full_cil = Sc_C.Defs.parse_file full_c
  and lib_cil = Sc_C.Defs.parse_file lib_c in
  let cil = Sc_C.Defs.diff_globals full_cil lib_cil in
  Lwt.return (full_cil, cil)

let add_cil_var_attributes ptr_config cil_var =
  let varname = Sc_C.Defs.var_name cil_var in
  let size =
    (* Calculating size if constrained by array_size_mapping *)
    match Sc_C.Named_loc.find_var ~varname ptr_config.array_size_mapping with
    | Some v -> Some (`Var v)
    | None -> None
  and carray =
    Sc_C.Named_loc.var_mem varname ptr_config.treat_pointer_as_array
  and cstring =
    Sc_C.Named_loc.var_mem varname ptr_config.treat_pointer_as_cstring
  in
  match size, carray, cstring with           (* carray if in size mapping *)
  | Some s,  _,    _ -> Sc_C.Defs.as_pointer_to_carray ~size:s cil_var
  | None, true,    _ -> Sc_C.Defs.as_pointer_to_carray cil_var
  | None,    _, true -> Sc_C.Defs.as_pointer_to_cstring cil_var
  | None,    _,    _ -> cil_var

(** Adds attributes to function parameters based on given pointer constraints
    stored in [config] (treat_pointer_as_array/cstring & array_size_mapping) *)
(* /!\ Non-variable constraints (contraints on pointers of pointers for
   example) are not stored in attributes. *)
let add_var_attributes ~config func_repr =
  Sc_C.Defs.map_func_inputs func_repr ~f:begin fun _ v ->
    add_cil_var_attributes config.project_pointer_handling v
  end

let add_struct_type_attributes ~config (compinfo: Cil.compinfo) =
  let struct_name = compinfo.cname in
  let cfields =
    List.map begin fun (Cil.{ fname = field_name; _ } as field) ->
      let size =
        Sc_C.Named_loc.find_field ~struct_name ~field_name
          config.project_pointer_handling.array_size_mapping
      and carray =
        Sc_C.Named_loc.field_mem ~struct_name ~field_name
          config.project_pointer_handling.treat_pointer_as_array
      and cstring =
        Sc_C.Named_loc.field_mem ~struct_name ~field_name
          config.project_pointer_handling.treat_pointer_as_cstring
      in
      match size, carray, cstring with
      | Some f,  _,    _ ->
          Sc_C.Defs.as_pointer_field_to_carray_field ~size:(`Var f) field
      | None, true,    _ ->
          Sc_C.Defs.as_pointer_field_to_carray_field field
      | None,    _, true ->
          Sc_C.Defs.as_pointer_field_to_cstring_field field
      | None,    _,    _ ->
          field
    end compinfo.cfields
  in
  { compinfo with cfields }

let add_global_type_attributes ~config cil =
  Sc_C.Defs.replace_globals cil ~f:begin function
    | GCompTag ({ cstruct; _ } as s, loc) when cstruct ->
        GCompTag (add_struct_type_attributes ~config s, loc)
    | x ->
        x
  end

let rec voidp_: Cil.typ -> bool = function
  | TPtr (TVoid _, _) -> true
  | TPtr (t, _) -> voidp_ t
  | _ -> false

let elaboration_error e =
  raise @@ ELABORATION_ERROR e

let check_entrypoint_func ~config (func: Sc_C.Types.func_repr) =
  let invalid_formals = List.filter (fun (t, _, _) -> voidp_ t) func.func_args in
  let kept_globals =
    (* Note: at the moment we do not report members of ignored_globals that do
       not correspond to any global variable from the program. *)
    List.filter
      (fun (_, v, _) -> not (Strings.mem v config.project_problem.ignored_globals))
      func.func_env.glob_vars
  in
  let discarded_globals, kept_globals =
    List.partition (fun (t, _, _) -> voidp_ t) kept_globals
  in
  List.iter begin fun (t, f, _) ->
    (* Later: return as symbolic warnings to be displayed at application-level *)
    Log.warn "Ignoring@ global@ variable@ `%a'@ as it has@ an@ unsupported@ type"
      Sc_values.Printer.cil_decl (t, f);
  end discarded_globals;
  let _const_globals, kept_globals =
    List.partition (fun (t, _, _) -> Sc_C.Defs.const_typ t) kept_globals
  in
  if invalid_formals <> [] then begin
    let formals =
      Strings.of_seq @@
      Seq.map (fun (_, f, _) -> f) @@
      List.to_seq invalid_formals
    in
    elaboration_error @@ Unsupported_formals { formals; func }
  end;
  { func with func_env = { glob_vars = kept_globals } }

let check_init_func (func: Sc_C.Types.func_repr) =
  if func.func_args <> [] then
    elaboration_error @@ Unexpected_initialization_function_with_args { func };
  func

let test_struct ~typdecls (Sc_C.Types.{ func_name; _ } as func) =
  try
    Sc_values.Struct.from_cil_fields ~typdecls
      (Format.asprintf "__%s_inputs" func_name)
      (Sc_C.Defs.func_inputs func)
  with Sc_values.TYPES.SPECIFICATION_ERROR Unknown_field { field_name; _ } ->
    elaboration_error
      (Unknown_formals { formals = Strings.singleton field_name; func })

let setup_for ~config ~test_repr ~c_file =
  let* full_cil, cil = extract_cils c_file in
  patch_cil_file_types full_cil;
  let cil_typing_info = patch_and_inspect_cil_file_types cil in
  (* TODO: add Sc_values-specific attributes to some of the types *)
  add_global_type_attributes ~config full_cil;    (* Note: after CIL patching *)
  let typdecls = Sc_values.typdecls_from_cil_file full_cil in
  (* TODO: extract globals/function environment from the function
     representation. *)
  let func_repr = Sc_C.Defs.cil_func cil config.project_problem.entrypoint in
  let init_func =
    match config.project_problem.initialization_function with
    | "" -> None
    | fn -> Some (check_init_func @@ Sc_C.Defs.cil_func cil fn)
  and oracle_func =
    match config.project_problem.oracle_function with
    | "" -> None
    | fn -> Some (Sc_C.Defs.cil_func cil fn)             (* TODO: check_oracle *)
  in
  (* check_array_size_mapping ~config func_repr; *)
  let func_repr = add_var_attributes ~config func_repr in
  let func_repr = check_entrypoint_func ~config func_repr in
  let test_struct = test_struct ~typdecls func_repr in
  let seek_oracle_failures = config.project_problem.seek_oracle_failures in
  Lwt.return { typdecls; func_repr; cil; test_struct; test_repr;
               init_func; oracle_func; cil_typing_info; seek_oracle_failures }
