  $ seacoral --tools libfuzzer
  [A]{Sc} Starting to log into `_sc/tritype.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `tritype'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `tritype':
  [A]{Sc} - Tests were generated
  [A]{Sc} Skipped reporting of lreplay results

  $ seacoral --tools libfuzzer --inputs tritype-with-preconditions.c
  [A]{Sc} Starting to log into `_sc/tritype-with-preconditions.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `tritype'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `tritype':
  [A]{Sc} - Tests were generated
  [A]{Sc} Skipped reporting of lreplay results

This part of the test may fail due to a mismatch between coverage
reported by Lreplay and that of the store. This is possibly due to a
mismatch in handling of NANs between clang-compiled fuzzed code and
validator on one side, and gcc-compiled code by Lreplay.
  $ seacoral --tools libfuzzer --inputs tritype-double.c >/dev/null
