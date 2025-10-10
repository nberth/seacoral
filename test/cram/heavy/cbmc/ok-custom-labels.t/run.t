  $ seacoral --tools cbmc --cbmc-mode cover --clean-start
  [A]{Sc} Starting to log into `_sc/custom.c-Custom-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `f'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `f':
          cov: 1 (33.3%) uncov: 0 (0.0%) unkwn: 2 (66.7%) with 1 test
  [A]{Sc} Covered labels: {1}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Unknown
                 3: Unknown
          Coverage: (1/3) 33.3%
  $ seacoral --tools cbmc --cbmc-mode assert --clean-start
  [A]{Sc} Starting to log into `_sc/custom.c-Custom-@2/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `f'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `f':
          cov: 1 (33.3%) uncov: 2 (66.7%) unkwn: 0 (0.0%) with 1 test
  [A]{Sc} Covered labels: {1}
  [A]{Sc} Uncoverable labels: {2, 3}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Unknown
                 3: Unknown
          Coverage: (1/3) 33.3%
  $ seacoral --tools cbmc --cbmc-mode clabel --clean-start
  [A]{Sc} Starting to log into `_sc/custom.c-Custom-@3/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `f'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `f':
          cov: 1 (33.3%) uncov: 2 (66.7%) unkwn: 0 (0.0%) with 1 test
  [A]{Sc} Covered labels: {1}
  [A]{Sc} Uncoverable labels: {2, 3}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Unknown
                 3: Unknown
          Coverage: (1/3) 33.3%
