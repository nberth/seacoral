  $ seacoral --tools cbmc --config static.toml
  [A]{Sc} Starting to log into `_sc/static.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `static_'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `static_':
          cov: 4 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 3 tests
  [A]{Sc} Covered labels: {1, 2, 3, 4}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Covered
                 3: Covered
                 4: Covered
          Coverage: (4/4) 100.0%
  $ seacoral --tools cbmc --config dynamic.toml
  [A]{Sc} Starting to log into `_sc/dynamic.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `dynamic'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `dynamic':
          cov: 6 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 4 tests
  [A]{Sc} Covered labels: {1, 2, 3, 4, 5, 6}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Covered
                 3: Covered
                 4: Covered
                 5: Covered
                 6: Covered
          Coverage: (6/6) 100.0%
  $ seacoral --tools cbmc --config dynamic_rev.toml
  [A]{Sc} Starting to log into `_sc/dynamic_rev.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `dynamic_rev'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `dynamic_rev':
          cov: 6 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 4 tests
  [A]{Sc} Covered labels: {1, 2, 3, 4, 5, 6}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Covered
                 3: Covered
                 4: Covered
                 5: Covered
                 6: Covered
          Coverage: (6/6) 100.0%
  $ seacoral --tools cbmc --config null-or-empty.toml
  [A]{Sc} Starting to log into `_sc/null-or-empty.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `null_or_empty'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `null_or_empty':
          cov: 4 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 3 tests
  [A]{Sc} Covered labels: {1, 2, 3, 4}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Covered
                 3: Covered
                 4: Covered
          Coverage: (4/4) 100.0%
  $ seacoral --tools cbmc --config length5.toml
  [A]{Sc} Starting to log into `_sc/length5.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `length5'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `length5':
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
