  $ seacoral --tools klee,luncov --files fail-n-uncov.c
  [A]{Sc} Starting to log into `_sc/fail-n-uncov.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee and luncov in parallel on `neg'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `neg':
          cov: 0 (0.0%) uncov: 1 (50.0%) unkwn: 1 (50.0%) with 0 test
  [A]{Sc} Covered labels: {}
  [A]{Sc} Uncoverable labels: {2}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc} Oracle statistics: fails: 1 test
  [A]{Sc}        1: Unknown
                 2: Uncoverable
          Coverage: (0/2) 50.0%
  $ seacoral --tools klee,luncov --files fail-n-uncov.c --ignore-oracle-failures
  [A]{Sc} Starting to log into `_sc/fail-n-uncov.c-DC-@2/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee and luncov in parallel on `neg'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `neg':
          cov: 1 (50.0%) uncov: 1 (50.0%) unkwn: 0 (0.0%) with 1 test
  [A]{Sc} Covered labels: {1}
  [A]{Sc} Uncoverable labels: {2}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Uncoverable
          Coverage: (1/2) 100.0%
