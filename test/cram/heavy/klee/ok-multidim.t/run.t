  $ seacoral --config multilength5-static.toml --tools klee
  [A]{Sc} Starting to log into `_sc/multilength5-static.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `length55'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `length55':
          cov: 8 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 3 tests
  [A]{Sc} Covered labels: {1, 2, 3, 4, 5, 6, 7, 8}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Covered
                 3: Covered
                 4: Covered
                 5: Covered
                 6: Covered
                 7: Covered
                 8: Covered
          Coverage: (8/8) 100.0%
