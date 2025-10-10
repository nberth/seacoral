  $ seacoral --tools klee
  [A]{Sc} Starting to log into `_sc/get_sign.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `get_sign'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `get_sign':
          cov: 4 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 3 tests
  [A]{Sc} Covered labels: {1, 2, 3, 4}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Covered
                 3: Covered
                 4: Covered
          Coverage: (4/4) 100.0%
