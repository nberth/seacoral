(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

type t =
  {
    file: [`stat] Sc_sys.File.t;
    tools: tool_stats Basics.StrTbl.t;
  }

and tool_stats =
  {
    time: float;
    status: (unit, string) Result.t;                 (* TODO: anything better *)
    tests_generated: int;
  }

let of_list l =
  let tbl = Basics.StrTbl.create 1 in
  List.iter (fun (k, b) -> Basics.StrTbl.replace tbl k b) l;
  tbl

let to_list m = Basics.StrTbl.fold (fun k b acc -> (k, b) :: acc) m []

module Encoding = struct

  open Json_encoding

  let status =
    conv
      (function | Ok () -> "ok" | Error s -> s)
      (function "ok" -> Ok () | s -> Error s)
      string

  let tool_stats =
    conv
      begin
        fun {time; status; tests_generated} ->
          (time, status, tests_generated)
      end
      begin
        fun (time, status, tests_generated) ->
          {time; status; tests_generated}
      end
      (obj3
         (req "time" float)
         (req "status" status)
         (req "tests" int))

  let stats =
    conv
      to_list
      of_list
      (list @@ obj2
         (req "tool" string)
         (req "stats" tool_stats))
end

let is_stat_file f =
  Filename.extension (Sc_sys.File.name f) = ".json"

let make ~dir ~run_num : t =
  let file = Sc_sys.File.PRETTY.assume_in ~dir:dir "%i.json" run_num in
  let tools = Basics.StrTbl.create 1 in
  {file; tools}

let report ~stats ~tool (d: tool_stats) : unit =
  Basics.StrTbl.replace stats.tools tool d

let save stats =
  let () = Sc_sys.File.touch stats.file in
  let json = Json_encoding.construct Encoding.stats stats.tools in
  Yojson.Safe.to_file (Sc_sys.File.name stats.file) @@ Json_repr.to_yojson json

let load_from ~dir : t list Lwt.t =
  let files = Sc_sys.Lwt_file.files_of_dir dir in
  Lwt_stream.fold (fun file acc ->
      if is_stat_file file then
        let yoj = Yojson.Safe.from_file (Sc_sys.File.name file) in
        let json = Json_repr.from_yojson yoj in
        let tools = Json_encoding.destruct Encoding.stats json in
        {file; tools} :: acc
      else acc
    )
    files
    []

let result_to_str = function | Ok () -> "Ok" | Error s -> "Error: " ^ s

let print_bold_str =
  Fmt.(styled `Bold @@ string)

let print_stats fmt stats =
  Format.fprintf fmt
    "%a: %fs@,\
     %a: %s@,\
     %a: %i"
    print_bold_str "Time" stats.time
    print_bold_str "Status" (result_to_str stats.status)
    print_bold_str "Tests generated" stats.tests_generated

let print fmt t =
  Basics.StrTbl.iter begin fun tool stats ->
    Format.fprintf fmt
      "Statistics for tool %a:@,\
       @[<v 2>  %a@,@]@,"
      print_bold_str tool
      print_stats stats
  end t.tools
