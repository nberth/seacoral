Klee on the struct version fails for an unknown reason
  $ seacoral --tools klee
  [A]{Sc} Starting to log into `_sc/tritype.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `tritype'
  [E]{Sc_klee} Error while working with klee: SIGNALED(-1)
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `tritype':
          cov: 0 (0.0%) uncov: 0 (0.0%) unkwn: 20 (100.0%) with 0 test
  [A]{Sc} Covered labels: {}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Unknown
                 2: Unknown
                 3: Unknown
                 4: Unknown
                 5: Unknown
                 6: Unknown
                 7: Unknown
                 8: Unknown
                 9: Unknown
                10: Unknown
                11: Unknown
                12: Unknown
                13: Unknown
                14: Unknown
                15: Unknown
                16: Unknown
                17: Unknown
                18: Unknown
                19: Unknown
                20: Unknown
          Coverage: (0/20) 0.0%
