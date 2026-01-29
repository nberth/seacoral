  $ seacoral --tools cbmc --config simple.toml
  [A]{Sc} Starting to log into `_sc/simple.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `simple'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `simple':
          cov: 7 (70.0%) uncov: 0 (0.0%) unkwn: 3 (30.0%) with 3 tests
  [A]{Sc} Covered labels: {1, 3, 4, 5, 7, 8, 10}
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
                 9: Unknown
                10: Covered
          Coverage: (7/10) 70.0%

Note: CBMC integration fails this case as harness generation assumes the size field comes BEFORE the pointer field when visited in Sc_values.
  $ seacoral --tools cbmc --config simple-rev.toml
  [A]{Sc} Starting to log into `_sc/simple-rev.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `simple'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `simple':
          cov: 0 (0.0%) uncov: 0 (0.0%) unkwn: 10 (100.0%) with 0 test
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
          Coverage: (0/10) 0.0%
  $ seacoral --tools cbmc --config complex.toml
  [A]{Sc} Starting to log into `_sc/complex.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `complex'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `complex':
          cov: 9 (75.0%) uncov: 0 (0.0%) unkwn: 3 (25.0%) with 4 tests
  [A]{Sc} Covered labels: {1, 2, 3, 5, 6, 7, 9, 10, 12}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Covered
                 3: Covered
                 4: Unknown
                 5: Covered
                 6: Covered
                 7: Covered
                 8: Unknown
                 9: Covered
                10: Covered
                11: Unknown
                12: Covered
          Coverage: (9/12) 75.0%
