  $ seacoral --tools libfuzzer --config static.toml
  [A]{Sc} Starting to log into `_sc/static.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `static_'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `static_':
  [A]{Sc} - Tests were generated
  [A]{Sc} Skipped reporting of lreplay results
  $ seacoral --tools libfuzzer --config dynamic.toml
  [A]{Sc} Starting to log into `_sc/dynamic.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `dynamic'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `dynamic':
  [A]{Sc} - Tests were generated
  [A]{Sc} Skipped reporting of lreplay results
  $ seacoral --tools libfuzzer --config dynamic_rev.toml
  [A]{Sc} Starting to log into `_sc/dynamic_rev.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `dynamic_rev'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `dynamic_rev':
  [A]{Sc} - Tests were generated
  [A]{Sc} Skipped reporting of lreplay results
  $ seacoral --tools libfuzzer --config null-or-empty.toml
  [A]{Sc} Starting to log into `_sc/null-or-empty.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `null_or_empty'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `null_or_empty':
  [A]{Sc} - Tests were generated
  [A]{Sc} Skipped reporting of lreplay results
  $ seacoral --tools libfuzzer --config length5.toml
  [A]{Sc} Starting to log into `_sc/length5.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `length5'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `length5':
  [A]{Sc} - Tests were generated
  [A]{Sc} Skipped reporting of lreplay results
