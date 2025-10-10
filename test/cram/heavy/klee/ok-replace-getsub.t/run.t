  $ seacoral --tools klee
  [A]{Sc} Starting to log into `_sc/replace.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `getsub'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `getsub':
          cov: 10 (50.0%) uncov: 0 (0.0%) unkwn: 10 (50.0%) with 3 tests
  [A]{Sc} Covered labels: {2, 3, 11, 12, 13, 14, 16, 18, 19, 20}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: 2 tests
  [A]{Sc}        1: Unknown
                 2: Covered
                 3: Covered
                 4: Unknown
                 5: Unknown
                 6: Unknown
                 7: Unknown
                 8: Unknown
                 9: Unknown
                10: Unknown
                11: Covered
                12: Covered
                13: Covered
                14: Covered
                15: Unknown
                16: Covered
                17: Unknown
                18: Covered
                19: Covered
                20: Covered
          Coverage: (10/20) 50.0%
