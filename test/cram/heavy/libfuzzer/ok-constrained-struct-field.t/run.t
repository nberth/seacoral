  $ seacoral --tools libfuzzer --config simple.toml
  [A]{Sc} Starting to log into `_sc/simple.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `simple'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `simple':
  [A]{Sc} - Tests were generated
  [A]{Sc} Skipped reporting of lreplay results

  $ seacoral --tools libfuzzer --config simple-rev.toml
  [A]{Sc} Starting to log into `_sc/simple-rev.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `simple'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `simple':
  [A]{Sc} - Tests were generated
  [A]{Sc} Skipped reporting of lreplay results

  $ seacoral --tools libfuzzer --config complex.toml
  [A]{Sc} Starting to log into `_sc/complex.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `complex'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `complex':
  [A]{Sc} - Tests were generated
  [A]{Sc} - Some crashes where found
  [A]{Sc} Skipped reporting of lreplay results

  $ seacoral --tools libfuzzer --config struct.toml
  [A]{Sc} Starting to log into `_sc/struct.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `struct_'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `struct_':
  [A]{Sc} - Tests were generated
  [A]{Sc} - Some crashes where found
  [A]{Sc} Skipped reporting of lreplay results
