TODO: We need to distinguish label-covering tests from RTE-triggering
tests in final count.
  $ seacoral --tools klee
  [A]{Sc} Starting to log into `_sc/null.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `segfault'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `segfault':
          cov: 2 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 2 tests
  [A]{Sc} Covered labels: {1, 2}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: 2 tests
  [A]{Sc}        1: Covered
                 2: Covered
          Coverage: (2/2) 100.0%
