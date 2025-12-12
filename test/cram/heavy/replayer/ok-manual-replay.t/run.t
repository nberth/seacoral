  $ seacoral replay
  [A]{Sc} Starting to log into `_sc/test.c-MCC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching test-runner on `f'
  [W]{Sc_replayer} Test runner has no file to run. Did you forget to import
                   test files in the "test-runner" configuration section?
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `f':
          cov: 0 (0.0%) uncov: 0 (0.0%) unkwn: 4 (100.0%) with 0 test
  [A]{Sc} Covered labels: {}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Unknown
                 2: Unknown
                 3: Unknown
                 4: Unknown
          Coverage: (0/4) 0.0%
Test 3 is redundant with test test 4, so one of them is discarded
  $ seacoral replay --test-runner-custom-tests="replayed/test1.c,replayed/test2.c,replayed/test3.c,replayed/test4.c"
  [A]{Sc} Starting to log into `_sc/test.c-MCC-@2/logs/2.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Current coverage statistics for `f':
          cov: 0 (0.0%) uncov: 0 (0.0%) unkwn: 4 (100.0%) with 0 test
          rte: none
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching test-runner on `f'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `f':
          cov: 3 (75.0%) uncov: 0 (0.0%) unkwn: 1 (25.0%) with
          3 tests (3 imported)
  [A]{Sc} Covered labels: {1, 3, 4}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Unknown
                 3: Covered
                 4: Covered
          Coverage: (3/4) 75.0%
  $ seacoral replay --test-runner-custom-tests="replayed/test5.c"
  [A]{Sc} Starting to log into `_sc/test.c-MCC-@3/logs/3.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Current coverage statistics for `f':
          cov: 3 (75.0%) uncov: 0 (0.0%) unkwn: 1 (25.0%) with
          3 tests (3 imported)
          rte: none
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching test-runner on `f'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `f':
          cov: 4 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with
          4 tests (4 imported)
  [A]{Sc} Covered labels: {1, 2, 3, 4}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Covered
                 3: Covered
                 4: Covered
          Coverage: (4/4) 100.0%
