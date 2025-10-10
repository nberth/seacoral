  $ seacoral --tools libfuzzer --files ok.c
  [A]{Sc} Starting to log into `_sc/ok.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `neg'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `neg':
  [A]{Sc} - NO test was generated
  [A]{Sc} - NO label was covered
  [A]{Sc} - NO oracle failure was found
  [A]{Sc} Skipped reporting of lreplay results
  $ seacoral --tools libfuzzer --files fail.c
  [A]{Sc} Starting to log into `_sc/fail.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `neg'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `neg':
  [A]{Sc} - NO test was generated
  [A]{Sc} - NO label was covered
  [A]{Sc} - Some oracle failures where found
  [A]{Sc} Skipped reporting of lreplay results
  $ seacoral --tools libfuzzer --files uncov.c
  [A]{Sc} Starting to log into `_sc/uncov.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `neg'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `neg':
  [A]{Sc} - Tests were generated
  [A]{Sc} - NO oracle failure was found
  [A]{Sc} Skipped reporting of lreplay results
  $ seacoral --tools libfuzzer --files fail-n-uncov.c
  [A]{Sc} Starting to log into `_sc/fail-n-uncov.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `neg'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `neg':
  [A]{Sc} - NO test was generated
  [A]{Sc} - NO label was covered
  [A]{Sc} - Some oracle failures where found
  [A]{Sc} Skipped reporting of lreplay results
  $ seacoral --tools libfuzzer --files fail-n-uncov.c --ignore-oracle-failures
  [A]{Sc} Starting to log into `_sc/fail-n-uncov.c-DC-@2/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `neg'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `neg':
  [A]{Sc} - Tests were generated
  [A]{Sc} Skipped reporting of lreplay results
