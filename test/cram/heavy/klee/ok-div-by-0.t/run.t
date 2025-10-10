  $ seacoral --tools klee
  [A]{Sc} Starting to log into `_sc/div0.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `div0'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `div0':
          cov: 2 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 2 tests
  [A]{Sc} Covered labels: {1, 2}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: 1 test
  [A]{Sc}        1: Covered
                 2: Covered
          Coverage: (2/2) 100.0%

  $ seacoral --tools klee --inputs div0-with-precondition.c
  [A]{Sc} Starting to log into `_sc/div0-with-precondition.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `div0'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `div0':
          cov: 1 (50.0%) uncov: 0 (0.0%) unkwn: 1 (50.0%) with 1 test
  [A]{Sc} Covered labels: {1}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: 1 test
  [A]{Sc}        1: Covered
                 2: Unknown
          Coverage: (1/2) 50.0%
