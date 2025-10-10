Check what happens when no valid seed is provided. Here, we
purposefully restrict seeds to a single zero-uniform byte array that
triggers an error in the seed triage phase. As a result, the fuzzing
phase should start wihtout any valid seed.
  $ seacoral --tools libfuzzer --libfuzzer-init-corpus-size 1 --libfuzzer-init-corpus-with-uniform-bytes="\\x00" --libfuzzer-max-starvation-time 1
  [A]{Sc} Starting to log into `_sc/test.c-WM-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `foo'
  [W]{Sc_fuzz.Libfuzzer} No working seed found: next retry in 1 second.
  [W]{Sc_fuzz.Libfuzzer} No working seed found: fuzzing skipped.
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `foo':
  [A]{Sc} - NO test was generated
  [A]{Sc} - NO label was covered
  [A]{Sc} Skipped reporting of lreplay results
