  $ seacoral --config math.toml --tool cbmc
  [A]{Sc} Starting to log into `_sc/math.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `obfuscated_one'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `obfuscated_one':
          cov: 2 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 2 tests
  [A]{Sc} Covered labels: {1, 2}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Covered
          Coverage: (2/2) 100.0%
  $ seacoral --config math.toml --tool cbmc --cbmc-mode assert --clean-start
  [A]{Sc} Starting to log into `_sc/math.c-DC-@2/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `obfuscated_one'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `obfuscated_one':
          cov: 2 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 2 tests
  [A]{Sc} Covered labels: {1, 2}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Covered
          Coverage: (2/2) 100.0%
