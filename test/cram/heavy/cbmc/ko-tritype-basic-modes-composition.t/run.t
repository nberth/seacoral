This is considered to fail, as launching in cover mode after assert
mode should achieve as well as cover (that finds 100% coverage).

  $ seacoral --tools cbmc --cbmc-mode assert
  [A]{Sc} Starting to log into `_sc/tritype.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `tritype'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `tritype':
          cov: 19 (95.0%) uncov: 0 (0.0%) unkwn: 1 (5.0%) with 8 tests
  [A]{Sc} Covered labels:
          {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 20}
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
                12: Unknown
                13: Covered
                14: Covered
                15: Covered
                16: Covered
                17: Covered
                18: Covered
                19: Covered
                20: Covered
          Coverage: (19/20) 95.0%

  $ seacoral --tools cbmc --cbmc-mode cover
  [A]{Sc} Starting to log into `_sc/tritype.c-DC-@2/logs/2.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Current coverage statistics for `tritype':
          cov: 19 (95.0%) uncov: 0 (0.0%) unkwn: 1 (5.0%) with 8 tests
          rte: none
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `tritype'
  [W]{Sc_cbmc} Found no properties to verify on the C file
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `tritype':
          cov: 19 (95.0%) uncov: 0 (0.0%) unkwn: 1 (5.0%) with 8 tests
  [A]{Sc} Covered labels:
          {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 20}
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
                12: Unknown
                13: Covered
                14: Covered
                15: Covered
                16: Covered
                17: Covered
                18: Covered
                19: Covered
                20: Covered
          Coverage: (19/20) 95.0%
