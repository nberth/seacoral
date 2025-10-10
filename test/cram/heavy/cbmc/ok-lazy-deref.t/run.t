  $ seacoral --tools cbmc
  [A]{Sc} Starting to log into `_sc/lazy-deref.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `f'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `f':
          cov: 6 (75.0%) uncov: 0 (0.0%) unkwn: 2 (25.0%) with 3 tests
  [A]{Sc} Covered labels: {1, 3, 4, 5, 7, 8}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Unknown
                 3: Covered
                 4: Covered
                 5: Covered
                 6: Unknown
                 7: Covered
                 8: Covered
          Coverage: (6/8) 75.0%
