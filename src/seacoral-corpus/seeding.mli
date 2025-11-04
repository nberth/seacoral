(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2025 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Affero General    *)
(*  Public License.                                                       *)
(*                                                                        *)
(**************************************************************************)

(** [init_seeds test_repr ~corpus ~max_failed_attempts ~init_corpus_size
    ~init_corpus_with_uniform_bytes dir] initializes a {e local} corpus
    directory [dir], and returns a structure with both the number of imported
    tests and the number of tests effectively generated in this new corpus.

    [dir] is first filled with every label-covering test that has been
    registered in [corpus].

    Then, if [init_corpus_size] is not reached, a number of uniform tests are
    inserted into the local corpus.  These tests are constructed according to
    the following process: for each byte [b] in
    [init_corpus_with_uniform_bytes], considered in order (this list is empty by
    default), build a test by decoding a string of [n] bytes [b] where [n] is
    the maximum size of the serialized representation of tests.

    At last, if [init_corpus_size] is still not reached, a number of randomly
    generated tests are inserted.

    Generation stops after [max_failed_attempts] failed generation attempts
    (when a generated test already belongs to the local corpus). *)
val init_seeds
  : 'raw_test Types.repr
  -> corpus:'raw_test Main.corpus
  -> ?max_failed_attempts:int
  -> init_corpus_size:int
  -> ?init_corpus_with_uniform_bytes:char list
  -> Sc_sys.File.dir
  -> Types.seeding_info Lwt.t
