  $ export CBMC_MODE=cover
  $ seacoral --tools cbmc
  [A]{Sc} Starting to log into `_sc/get_sign.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `get_sign'
  [A]{Sc_corpus.Validator} SC_EXACT_LABELS_FILE=$TESTCASE_ROOT/_sc/get_sign.c-CC-@1/validator/af2b716548ec79d2443e33ec19fdad9a
  [A]{Sc_corpus.Validator} SC_EXACT_LABELS_FILE=$TESTCASE_ROOT/_sc/get_sign.c-CC-@1/validator/eaba9fcedb3e6ae7b15d54b2174d54aa
  [A]{Sc_corpus.Validator} SC_EXACT_LABELS_FILE=$TESTCASE_ROOT/_sc/get_sign.c-CC-@1/validator/f1d3ff8443297732862df21dc4e57262
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `get_sign':
          cov: 4 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 3 tests
  [A]{Sc} Covered labels: {1, 2, 3, 4}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc} Test 1: covering labels {2, 4}
  [A]{Sc} Test 2: covering labels {2, 3}
  [A]{Sc} Test 3: covering labels {1}
  [A]{Sc}        1: Covered
                 2: Covered
                 3: Covered
                 4: Covered
          Coverage: (4/4) 100.0%
