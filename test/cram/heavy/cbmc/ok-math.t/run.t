  $ seacoral --config math.toml --tool cbmc
  [A]{Sc} Starting to log into `_sc/math.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `obfuscated_one'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `obfuscated_one':
          cov: 1 (50.0%) uncov: 0 (0.0%) unkwn: 1 (50.0%) with 1 test
  [A]{Sc} Covered labels: {2}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Unknown
                 2: Covered
          Coverage: (1/2) 50.0%
  $ seacoral --config math.toml --tool cbmc --cbmc-mode assert --clean-start
  [A]{Sc} Starting to log into `_sc/math.c-DC-@2/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `obfuscated_one'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `obfuscated_one':
          cov: 1 (50.0%) uncov: 0 (0.0%) unkwn: 1 (50.0%) with 1 test
  [A]{Sc} Covered labels: {2}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Unknown
                 2: Covered
          Coverage: (1/2) 50.0%
