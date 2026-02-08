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

(* Preliminary utilities *)

let stdout_default = `Grab (Lines (Lwt_fmt.printf "+ %s@."))
let stderr_default = `Grab (Lines (Lwt_fmt.printf "* %s@."))

let error_default = Lwt_fmt.printf "%a" Sc_sys.Process.pp_unix_status

let sh ?(stdout = stdout_default) ?(stderr = stderr_default) ?timeout
    ?on_success ?(on_error = error_default) cmd =
  Sc_sys.Process.shell_unit cmd ~stdout ~stderr ~log_command:`On_error
    ?timeout ~on_error ?on_success

let bg ?(stdout = stdout_default) ?(stderr = stderr_default) ?timeout
    ~on_success ?(on_error = error_default) cmd =
  Sc_sys.Process.shell cmd ~stdout ~stderr ~log_command:`On_error ?timeout
    ~on_error ~on_success

let run ?(timeout = 1.) f =
  Lwt_main.run begin
    let* () =
      Lwt.catch begin fun () ->
        Lwt_unix.with_timeout timeout (fun () -> f)
      end begin function
        | Lwt_unix.Timeout ->
            Lwt_fmt.eprintf "Test timed out"
        | e ->
            Lwt.reraise e
      end
    in
    Lwt.join Lwt_fmt.[flush stdout; flush stderr]
  end

(* Actual tests follow *)

let%expect_test "ok-delayed-output-retrieval-exit-0" =
  run begin
    let stdout_lines, new_stdout_line = Lwt_stream.create () in
    let* () = sh "echo a" ~stdout:(`Grab (Push_lines new_stdout_line)) in
    Gc.full_major ();         (* <- in case that has an adversarial effect... *)
    Lwt_stream.iter_s (Lwt_fmt.printf "%S@.") stdout_lines
  end;
  [%expect{| "a" |}]
;;

let%expect_test "ok-delayed-output-retrieval-exit-1" =
  run begin
    let stdout_lines, new_stdout_line = Lwt_stream.create () in
    let* () = sh "echo a; exit 1" ~stdout:(`Grab (Push_lines new_stdout_line)) in
    Gc.full_major ();         (* <- in case that has an adversarial effect... *)
    Lwt_stream.iter_s (Lwt_fmt.printf "%S@.") stdout_lines
  end;
  [%expect{| EXITED(1)"a" |}]
;;

let%expect_test "ok-delayed-output-retrieval-delayed-exit-1" =
  run begin
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
  [%expect{|
    "a"
    SIGNALED(-7)
    |}]
;;

let%expect_test "ok-join-strictly-after-stdout-consumption" =
  run begin
    let stdout_lines, new_stdout_line = Lwt_stream.create () in
    let* proc =
      bg "exit 0"
        ~stdout:(`Grab (Push_lines new_stdout_line))
        ~on_success:Lwt.return
        ~on_error:(fun _ -> Lwt_fmt.printf "errored@.")
    in
    let x = ref 0 in
    let* () = Lwt_stream.iter (fun _ -> incr x) stdout_lines in
    let* () = Sc_sys.Process.join proc in
    Lwt_fmt.printf "%d@." !x
  end;
  [%expect{| 0 |}]
;;

let%expect_test "ok-no-join-after-stdout-grab" =
  run begin
    let stdout_lines_mbox = Lwt_mvar.create_empty () in
    let* _proc =
      bg "echo a; echo b"
        ~stdout:(`Grab (Stream (Lwt_mvar.put stdout_lines_mbox)))
        ~on_success:Lwt.return
        ~on_error:(fun _ -> Lwt_fmt.printf "errored@.")
    and* stdout_lines =
      Lwt_mvar.take stdout_lines_mbox
    in
    let x = ref 0 in
    let* () = Lwt_stream.iter (fun _ -> incr x) stdout_lines in
    Lwt_fmt.printf "%d@." !x
  end;
  [%expect{| 2 |}]
;;
