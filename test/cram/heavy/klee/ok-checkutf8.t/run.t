  $ seacoral --tools klee
  [A]{Sc} Starting to log into `_sc/checkutf8.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `checkutf8'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `checkutf8':
          cov: 52 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 23 tests
  [A]{Sc} Covered labels:
          {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
           20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36,
           37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: 8 tests
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
                21: Covered
                22: Covered
                23: Covered
                24: Covered
                25: Covered
                26: Covered
                27: Covered
                28: Covered
                29: Covered
                30: Covered
                31: Covered
                32: Covered
                33: Covered
                34: Covered
                35: Covered
                36: Covered
                37: Covered
                38: Covered
                39: Covered
                40: Covered
                41: Covered
                42: Covered
                43: Covered
                44: Covered
                45: Covered
                46: Covered
                47: Covered
                48: Covered
                49: Covered
                50: Covered
                51: Covered
                52: Covered
          Coverage: (52/52) 100.0%
