  $ seacoral --tools libfuzzer
  [A]{Sc} Starting to log into `_sc/div0.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `div0'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `div0':
  [A]{Sc} - Tests were generated
  [A]{Sc} - Some crashes where found
  [A]{Sc} Skipped reporting of lreplay results

Note: `div0-with-precondition.c` prevents libfuzzer from starting if
every objective has been attained by other tools.
  $ seacoral --tools libfuzzer --inputs div0-with-precondition.c
  [A]{Sc} Starting to log into `_sc/div0-with-precondition.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `div0'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `div0':
  [A]{Sc} - NO test was generated
  [A]{Sc} - NO label was covered
  [A]{Sc} - Some crashes where found
  [A]{Sc} Skipped reporting of lreplay results
