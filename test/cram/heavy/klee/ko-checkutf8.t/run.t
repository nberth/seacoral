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
  [A]{Sc} Crash statistics: rte: 7 tests
  [A]{Sc}        1: Covered
                 2: Covered
                 3: Covered
                 4: Covered
                 5: Covered
                 6: Covered
                 7: Covered
                 8: Unknown
                 9: Covered
                10: Covered
                11: Covered
                12: Covered
                13: Covered
                14: Covered
                15: Covered
                16: Covered
                17: Covered
                18: Unknown
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
                32: Unknown
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
          Coverage: (49/52) 94.2%
  [W]{Sc} The set of covered labels reported by Lreplay does not match
          SeaCoral's own accounting
  [W]{Sc} - Reported covered in the store only: {8, 18, 32}
  [W]{Sc} Label-covering tests shown below...
  [W]{Sc} Test 30: {.buffer = &{240, 128, 128, 192}, .n = 4}
  [W]{Sc} Test 20: {.buffer = &{224, 128, 128}, .n = 3}
  [W]{Sc} Test 15: {.buffer = &{224, 0}, .n = 3}
  [W]{Sc} Test 2: {.buffer = &0, .n = 1}
  [W]{Sc} Test 28: {.buffer = &{240, 128, 128, 128}, .n = 4}
  [W]{Sc} Test 25: {.buffer = &{240, 128, 0}, .n = 4}
  [W]{Sc} Test 3: {.buffer = &{}, .n = 1}
  [W]{Sc} Test 17: {.buffer = &{224, 192}, .n = 3}
  [W]{Sc} Test 12: {.buffer = &{192, 192}, .n = 2}
  [W]{Sc} Test 26: {.buffer = &{238, 128, 128}, .n = 3}
  [W]{Sc} Test 8: {.buffer = &240, .n = 1}
  [W]{Sc} Test 23: {.buffer = &{224, 128, 192}, .n = 3}
  [W]{Sc} Test 21: {.buffer = &{224, 128, 0}, .n = 3}
  [W]{Sc} Test 6: {.buffer = &224, .n = 1}
  [W]{Sc} Test 27: {.buffer = &{240, 128, 192}, .n = 4}
  [W]{Sc} Test 10: {.buffer = &{192, 0}, .n = 2}
  [W]{Sc} Test 9: {.buffer = &{192, 128}, .n = 2}
  [W]{Sc} Test 1: {.buffer = &0, .n = 0}
  [W]{Sc} Test 29: {.buffer = &{240, 128, 128, 0}, .n = 4}
  [W]{Sc} Test 19: {.buffer = &{240, 0}, .n = 4}
  [W]{Sc} Test 13: {.buffer = &248, .n = 1}
  [W]{Sc} Test 5: {.buffer = &192, .n = 1}
  [W]{Sc} Test 22: {.buffer = &{240, 192}, .n = 4}
