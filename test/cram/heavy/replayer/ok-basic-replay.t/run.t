  $ seacoral --tools test-runner
  [A]{Sc} Starting to log into `_sc/test.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching test-runner on `test'
  [E]{Sc_replayer} Replay of test _sc/test.c-CC-@1/test-runner/test_of_test_failing-dir/test_of_test_failing.c failed
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `test':
          cov: 7 (87.5%) uncov: 0 (0.0%) unkwn: 1 (12.5%) with
          2 tests (2 imported)
  [A]{Sc} Covered labels: {2, 3, 4, 5, 6, 7, 8}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Unknown
                 2: Covered
                 3: Covered
                 4: Covered
                 5: Covered
                 6: Covered
                 7: Covered
                 8: Covered
          Coverage: (7/8) 87.5%

Side test: here we check that values passed via CLI arguments are
properly taken into account:
  $ seacoral --tools test-runner --test-runner-custom-tests=test_of_test.c --workdir _sc2
  [A]{Sc} Starting to log into `_sc2/test.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching test-runner on `test'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `test':
          cov: 4 (50.0%) uncov: 0 (0.0%) unkwn: 4 (50.0%) with
          1 test (1 imported)
  [A]{Sc} Covered labels: {2, 3, 5, 7}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Unknown
                 2: Covered
                 3: Covered
                 4: Unknown
                 5: Covered
                 6: Unknown
                 7: Covered
                 8: Unknown
          Coverage: (4/8) 50.0%
