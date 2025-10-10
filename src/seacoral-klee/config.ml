(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

module TYPES = struct
  type options =
    {
      timeout: float;
      only_new_tests: bool;
      keep_error_cases: bool;
      search: string list;
      label_handling: label_handling;
      replay: replay;
      replay_max_concurrency: int;   (* no max if = 0, delayed replays if < 0 *)
      libc: string
    }

  and replay =
    | Inhibit (** do not replay any test *)
    | Enable  (** on-the-fly replay if relevant (w.r.t label handling policy) *)
    | Delayed (** delay any replay until after the main klee process
                  terminates *)
    | Force   (** force on-the-fly replay, whatever the label handling policy *)

  and label_handling =
    | Ignore            (** [pc_label(e, id)] amounts to no C code *)
    | Naive             (** [pc_label(e, id)] amounts to [if e set_cover(id)] *)
    | Optimized of { benign_test_policy: benign_test_policy }

  (** how are tests that pass no label treated? *)
  and benign_test_policy =
    | Keep         (** generate and keep all tests *)
    | Reject       (** do not keep any test that does not reach a label *)
    | Avoid        (** do not generate any test that neither reaches a label nor
                       triggers a runtime error *)
end
open TYPES

type t = TYPES.options

let toolname = "klee"

let section =
  let default =
    {
      timeout = 30.;
      only_new_tests = true;
      keep_error_cases = true;
      search = [];
      label_handling = Optimized { benign_test_policy = Keep };
      replay = Enable;
      replay_max_concurrency = 0;
      libc = "none";
    }
  and parse_replay s = match String.lowercase_ascii s with
    | "no" | "none" | "inhibit" | "inhibited" -> Inhibit
    | "ok" | "yes" | "enable" | "enabled" -> Enable
    | "delayed" -> Delayed
    | "force" | "forced" -> Force
    | _ -> Fmt.invalid_arg "Unknown value for replay: %s" s
  and replay_to_str = function
    | Inhibit -> "inhibit"
    | Enable -> "enable"
    | Delayed -> "delayed"
    | Force -> "force"
  and official_replay_values =
    Basics.PPrt.UFmt.string_set ["inhibit"; "yes"; "delayed"; "force"]
  and parse_label_handling s = match String.lowercase_ascii s with
    | "none" | "ignore" ->
        Ignore
    | "naive" | "basic" ->
        Naive
    | "optimize" | "optim"
    | "optimize+keep" | "optim+keep"
    | "optimize+keepall" | "optim+keepall" ->
        Optimized { benign_test_policy = Keep }
    | "optimize+reject" | "optim+reject" ->
        Optimized { benign_test_policy = Reject }
    | "optimize+avoid" | "optim+avoid" ->
        Optimized { benign_test_policy = Avoid }
    | _ ->
        Fmt.invalid_arg "Unknown value for label-handling: %s" s
  and label_handling_to_str = function
    | Ignore -> "ignore"
    | Naive -> "naive"
    | Optimized { benign_test_policy = Reject } -> "optimize+reject"
    | Optimized { benign_test_policy = Avoid  } -> "optimize+avoid"
    | Optimized { benign_test_policy = Keep   } -> "optimize+keep"
  and official_label_handling_values =
    Basics.PPrt.UFmt.string_set
      ["ignore"; "naive"; "optimize"; "optimize+avoid"; "optimize+keepall"]
  in
  Sc_config.Section.define toolname ~default ~entries:Sc_config.Eztoml.[
      float
        ~key:"timeout"
        ~doc:"Sets the timeout of the tool, in seconds; 0 means no timeout \
              (default: %a)"
        ~default:default.timeout
        ~runtime:true
        (fun c timeout -> { c with timeout })
        (fun c -> c.timeout);
      bool
        ~key:"only-new-tests"
        ~doc:"Activate option '--only-output-states-covering-new' (default: %a)"
        ~default:true
        ~runtime:true
        (fun c only_new_tests -> { c with only_new_tests })
        (fun c -> c.only_new_tests);
      bool
        ~key:"keep-error-cases"
        ~doc:"Keep tests that are generated because they raise errors (default: \
              %a)"
        ~default:true
        ~runtime:true
        (fun c keep_error_cases -> { c with keep_error_cases })
        (fun c -> c.keep_error_cases);
      string_list
        ~key:"search"
        ~doc:"Specify klee search strategy (see `klee --help` for details, use \
              an empty listfor klee defaults --- default: %a)"
        ~default:default.search
        ~runtime:true
        (fun c search -> { c with search })
        (fun c -> c.search);
      string
        ~key:"label-handling"
        ~doc:("Label handling mode (default: %a; possible values: " ^^
              official_label_handling_values ^^ ")")
        ~check:(fun str -> try ignore @@ parse_label_handling str; true
                 with Invalid_argument _ -> false)
        ~check_descr:("expected value in " ^^ official_label_handling_values)
        ~default:"optimize"
        ~runtime:false
        (fun c str -> { c with label_handling = parse_label_handling str })
        (fun c -> label_handling_to_str c.label_handling);
      string
        ~key:"replay"
        ~doc:("On-the-fly replay of generated test cases (default: %a; possible \
               values: " ^^ official_replay_values ^^ ")")
        ~check:(fun str -> try ignore @@ parse_replay str; true
                 with Invalid_argument _ -> false)
        ~check_descr:("expected value in " ^^ official_replay_values)
        ~default:"yes"
        ~runtime:true
        (fun c str -> { c with replay = parse_replay str })
        (fun c -> replay_to_str c.replay);
      int'
        ~key:"replay-max-concurrency"
        ~doc:"Maximum number of replayers running concurrently. A null value \
              enables full parallelism (this is the default), and a negative \
              value disables on-the-fly replay of test cases"
        ~env:"KLEE_REPLAY_MAX_CONCURRENCY"
        ~default:default.replay_max_concurrency
        ~runtime:true
        (fun c replay_max_concurrency -> { c with replay_max_concurrency })
        (fun c -> c.replay_max_concurrency);
      string
        ~key:"libc"
        ~doc:"Choose libc version (default: %a)"
        ~runtime:true
        ~default:default.libc
        ~check:(function | "klee" | "uclibc" | "none" -> true | _ -> false)
        (fun c libc -> {c with libc})
        (fun c -> c.libc);
    ]
