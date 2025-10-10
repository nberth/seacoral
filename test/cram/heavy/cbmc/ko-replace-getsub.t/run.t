  $ seacoral --tools cbmc
  [A]{Sc} Starting to log into `_sc/replace.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `getsub'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `getsub':
          cov: 5 (25.0%) uncov: 0 (0.0%) unkwn: 15 (75.0%) with 1 test
  [A]{Sc} Covered labels: {2, 12, 16, 18, 20}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Unknown
                 2: Covered
                 3: Unknown
                 4: Unknown
                 5: Unknown
                 6: Unknown
                 7: Unknown
                 8: Unknown
                 9: Unknown
                10: Unknown
                11: Unknown
                12: Covered
                13: Unknown
                14: Unknown
                15: Unknown
                16: Covered
                17: Unknown
                18: Covered
                19: Unknown
                20: Covered
          Coverage: (5/20) 25.0%

Also perform an additional test with a wrong config that used to trigger an
error:
  $ seacoral --tools cbmc --config wrong-pointer-handling.toml
  [A]{Sc} Starting to log into `_sc/replace.c-DC-@2/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `getsub'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `getsub':
          cov: 5 (25.0%) uncov: 0 (0.0%) unkwn: 15 (75.0%) with 1 test
  [A]{Sc} Covered labels: {2, 12, 16, 18, 20}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Unknown
                 2: Covered
                 3: Unknown
                 4: Unknown
                 5: Unknown
                 6: Unknown
                 7: Unknown
                 8: Unknown
                 9: Unknown
                10: Unknown
                11: Unknown
                12: Covered
                13: Unknown
                14: Unknown
                15: Unknown
                16: Covered
                17: Unknown
                18: Covered
                19: Unknown
                20: Covered
          Coverage: (5/20) 25.0%
