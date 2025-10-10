(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.                                                    *)
(*                                                                        *)
(**************************************************************************)

open Basics
open Types
open Sc_sys.File.TYPES

open Lwt.Syntax
open Sc_sys.File.Syntax

module Log = (val (Ez_logs.from_src @@ Logs.Src.create "Sc_corpus.Seeding"))

let init_seeds
    (type raw_test)
    ((module Raw_test): raw_test repr)
    ~(corpus: raw_test Main.corpus)
    ?(max_failed_attempts = 42)
    ~init_corpus_size
    ?(init_corpus_with_uniform_bytes = [])
    (seedsdir: dir)
  =
  let test_struct = Main.test_struct corpus in
  (* First, remove every exising file in libfuzzer corpus (this could be
     optimized a bit, but is a simple approach to avoid having duplicates
     (re-)imported form the runtime database). *)
  Log.debug "Reinitializing corpus";
  let* () = Sc_sys.Lwt_file.unlink_files_of_dir seedsdir in
  let* local_corpus =
    Sharing.import_tests corpus seedsdir
      ~filter:(fun m -> m.outcome = Covering_label)
  in
  let num_imported = Digests.cardinal local_corpus in
  if num_imported > 1 then begin
    Log.info "Copied %i existing seed%s into `%a'"
      num_imported (if num_imported > 1 then "s" else "")
      Sc_sys.File.print seedsdir
  end;
  let _, nbytes = Raw_test.size_bounds test_struct in
  let rec make ?(failed_attempts = 0) ?(with_chars = []) i =
    (* Note: [failed_attempts] is there to avoid looping indefinitely while
       generating seeds: the space of all test inputs (post-canonicalization)
       may be of cardinality smaller than the number of seeds requested. *)
    (* Note: [with_nulls] instructs to generate a seed with only zeroes; it
       may only hold on the first call to [make]. *)
    let write_and_continue ?with_chars bytes =
      let str = Bytes.to_string bytes in
      let v = Raw_test.Val.of_string test_struct str in
      let v_str = Raw_test.Val.to_string v in
      let id = Digest.string v_str in
      let failed_attempts, i =
        if Digests.mem id local_corpus
        then failed_attempts + 1, i
        else
          (* We need to detect existing files here: canonicalization in
             `Test_inputs.from_string` may actually lead to a seed that
             already exists. *)
          try
            let> oc =
              Sc_sys.File.create_empty_in ~dir:seedsdir (Digest.to_hex id) in
            Raw_test.Val.write oc v;
            failed_attempts, i - 1
          with Sc_sys.File.UNEXPECTED _ ->
            failed_attempts + 1, i
      in
      make ~failed_attempts ?with_chars i
    in
    if i <= 0 || failed_attempts >= max_failed_attempts then begin
      if failed_attempts >= max_failed_attempts
      then Log.warn "Skipping@ generation@ of@ %d@ seeds@ after@ %u@ failed@ \
                     attempts." i failed_attempts;
      Lwt.return i
    end else match with_chars with
      | '\x00' :: with_chars ->
          Log.info "Generating inputs with %d null bytes." nbytes;
          write_and_continue (Bytes.make nbytes '\x00') ~with_chars
      | c :: with_chars ->
          Log.info "Generating inputs with %d bytes at %u." nbytes (Char.code c);
          write_and_continue (Bytes.make nbytes c) ~with_chars
      | [] ->
          Log.info "Generating inputs with %d random bytes." nbytes;
          let rand_byte _ = Random.int 256 |> char_of_int in
          write_and_continue (Bytes.init nbytes rand_byte)
  in
  let n = init_corpus_size - num_imported in
  let* remaining = make n ~with_chars:init_corpus_with_uniform_bytes in
  Lwt.return { num_imported;
               num_generated = max 0 (n - remaining) }
