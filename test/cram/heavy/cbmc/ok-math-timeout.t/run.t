  $ seacoral --config math.toml --tool cbmc --cbmc-timeout 0.1
  [A]{Sc} Starting to log into `_sc/math.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `obfuscated_one'
  [E]{Sc_cbmc} Input channel closed unexpectedly: this is likely due to CBMC
               reaching its timeout.
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `obfuscated_one':
          cov: 0 (0.0%) uncov: 0 (0.0%) unkwn: 2 (100.0%) with 0 test
  [A]{Sc} Covered labels: {}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Unknown
                 2: Unknown
          Coverage: (0/2) 0.0%
