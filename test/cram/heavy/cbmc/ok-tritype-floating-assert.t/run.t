  $ export CBMC_MODE=assert

We only check acheved coverage for these tests.
  $ export SC_ENABLE_DETAILED_STATS=no

Note: one label missing here (possibly an RTE on a trace --- could
depend on CBMC's version):
  $ seacoral --tools cbmc --inputs tritype-float.c
  [A]{Sc} Starting to log into `_sc/tritype-float.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `tritype'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `tritype':
  [A]{Sc} - Tests were generated
  [A]{Sc} Skipped reporting of lreplay results
  $ tr -d '\0' < _sc/last/store/store | wc -c
  19

  $ seacoral --tools cbmc --inputs tritype-double.c
  [A]{Sc} Starting to log into `_sc/tritype-double.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching cbmc on `tritype'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `tritype':
  [A]{Sc} - Tests were generated
  [A]{Sc} Skipped reporting of lreplay results
  $ tr -d '\0' < _sc/last/store/store | wc -c
  20
