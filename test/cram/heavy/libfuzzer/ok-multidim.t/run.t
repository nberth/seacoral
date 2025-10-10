  $ seacoral --config multilength5-static.toml --tools libfuzzer
  [A]{Sc} Starting to log into `_sc/multilength5-static.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `length55'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `length55':
  [A]{Sc} - Tests were generated
  [A]{Sc} Skipped reporting of lreplay results

Only check coverage reaches a base threshold (number of generated
tests may vary; best coverage is 8, sometimes we get lower coverage):
  $ test $(tr -d '\0' < _sc/last/store/store | wc -c) -ge 7
