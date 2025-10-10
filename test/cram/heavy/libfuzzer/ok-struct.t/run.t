  $ seacoral --tools libfuzzer --config basic.toml
  [A]{Sc} Starting to log into `_sc/basic.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `f'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `f':
  [A]{Sc} - Tests were generated
  [A]{Sc} Skipped reporting of lreplay results
  $ seacoral --tools libfuzzer --config with-array.toml
  [A]{Sc} Starting to log into `_sc/with-array.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `f'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `f':
  [A]{Sc} - Tests were generated
  [A]{Sc} Skipped reporting of lreplay results
  $ seacoral --tools libfuzzer --config array.toml
  [A]{Sc} Starting to log into `_sc/array.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `f'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `f':
  [A]{Sc} - Tests were generated
  [A]{Sc} Skipped reporting of lreplay results
