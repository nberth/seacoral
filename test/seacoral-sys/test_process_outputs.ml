(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

open Sc_sys.Process.TYPES

open Lwt.Syntax

let stdout_default = `Grab (Lines (Lwt_fmt.printf "+ %s@."))
let stderr_default = `Grab (Lines (Lwt_fmt.printf "* %s@."))

let sh ?(stdout = stdout_default) ?(stderr = stderr_default) ?timeout
    ?on_success cmd =
  Sc_sys.Process.shell_unit cmd ~stdout ~stderr ~log_command:`On_error ?timeout
    ~on_error:(fun s -> Lwt_fmt.printf "%a" Sc_sys.Process.pp_unix_status s)
    ?on_success

let bg ?(stdout = stdout_default) ?(stderr = stderr_default) ?timeout
    ~on_success cmd =
  Sc_sys.Process.shell cmd ~stdout ~stderr ~log_command:`On_error ?timeout
    ~on_error:(fun s -> Lwt_fmt.printf "%a" Sc_sys.Process.pp_unix_status s)
    ~on_success

let%expect_test "delayed-output-retrieval-exit-0" =
  Lwt_main.run begin
    let stdout_lines, new_stdout_line = Lwt_stream.create () in
    let* () = sh "echo a" ~stdout:(`Grab (Push_lines new_stdout_line)) in
    Gc.full_major ();         (* <- in case that has an adversarial effect... *)
    Lwt_stream.iter_s (Lwt_fmt.printf "%S@.") stdout_lines
  end;
  [%expect{| "a" |}]
;;

let%expect_test "delayed-output-retrieval-exit-1" =
  Lwt_main.run begin
    let stdout_lines, new_stdout_line = Lwt_stream.create () in
    let* () = sh "echo a; exit 1" ~stdout:(`Grab (Push_lines new_stdout_line)) in
    Gc.full_major ();         (* <- in case that has an adversarial effect... *)
    Lwt_stream.iter_s (Lwt_fmt.printf "%S@.") stdout_lines
  end;
  [%expect{| EXITED(1)"a" |}]
;;

let%expect_test "delayed-output-retrieval-delayed-exit-1" =
  Lwt_main.run begin
    let stdout_lines, new_stdout_line = Lwt_stream.create () in
    let* proc =
      bg "echo a; sleep 10; exit 1"
        ~timeout:0.1
        ~stdout:(`Grab (Push_lines new_stdout_line))
        ~on_success:(fun _ -> Lwt_fmt.printf "done")
    in
    Gc.full_major ();         (* <- in case that has an adversarial effect... *)
    Lwt.join [
      Lwt_stream.iter_s (Lwt_fmt.printf "%S@.") stdout_lines;
      Sc_sys.Process.join proc;
    ]
  end;
  [%expect{| "a" |}]
;;
