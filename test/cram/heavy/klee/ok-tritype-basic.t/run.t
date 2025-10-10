  $ seacoral --tools klee
  [A]{Sc} Starting to log into `_sc/tritype.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `tritype'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `tritype':
          cov: 20 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 8 tests
  [A]{Sc} Covered labels:
          {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
           20}
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
                 9: Covered
                10: Covered
                11: Covered
                12: Covered
                13: Covered
                14: Covered
                15: Covered
                16: Covered
                17: Covered
                18: Covered
                19: Covered
                20: Covered
          Coverage: (20/20) 100.0%

  $ seacoral --tools klee --inputs tritype-with-preconditions.c
  [A]{Sc} Starting to log into `_sc/tritype-with-preconditions.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `tritype'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `tritype':
          cov: 20 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 9 tests
  [A]{Sc} Covered labels:
          {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
           20}
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
                 9: Covered
                10: Covered
                11: Covered
                12: Covered
                13: Covered
                14: Covered
                15: Covered
                16: Covered
                17: Covered
                18: Covered
                19: Covered
                20: Covered
          Coverage: (20/20) 100.0%

Note: Klee appears a bit limited in this test:
  $ seacoral --tools klee --inputs tritype-double.c
  [A]{Sc} Starting to log into `_sc/tritype-double.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `tritype'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `tritype':
          cov: 1 (5.0%) uncov: 0 (0.0%) unkwn: 19 (95.0%) with 1 test
  [A]{Sc} Covered labels: {1}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
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
          Coverage: (1/20) 5.0%
