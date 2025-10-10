  $ export SC_ENABLE_DETAILED_STATS=no
  $ seacoral --tools klee --no-lreplay --eacsl
  [A]{Sc} Starting to log into `_sc/tritype.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `tritype'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `tritype':
  [A]{Sc} - Tests were generated
  [A]{Sc} Every test satisfies the E-ACSL specification

  $ seacoral --tools klee --no-lreplay --export-split-suite --eacsl --eacsl-mode parallel
  [A]{Sc} Starting to log into `_sc/tritype.c-CC-@2/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `tritype'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `tritype':
  [A]{Sc} - Tests were generated
  [A]{Sc} Every test satisfies the E-ACSL specification
