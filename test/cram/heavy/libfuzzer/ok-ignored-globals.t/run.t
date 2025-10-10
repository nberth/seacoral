  $ seacoral --tools libfuzzer
  [A]{Sc} Starting to log into `_sc/ignored-global.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `f'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `f':
  [A]{Sc} - Tests were generated
  [A]{Sc} Skipped reporting of lreplay results

Check that `__unused_global` is not included in the test structure:
  $ grep 'struct __f_inputs {' _sc/last/decoder/decoder.h
  struct __f_inputs { int b;  };
