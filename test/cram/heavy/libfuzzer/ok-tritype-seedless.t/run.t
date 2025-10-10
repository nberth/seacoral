We check the behavior of libfuzzer with no viable seed.  We first
check that the fuzzer terminates properly: it used to enter an
infinite loop due to a crashing input (via `exit(1)` in an
`sc_assume`) and no measured progress on coverage via its internal
counters.  We impose a hard timout on the command in case it doesn't.

Interestingly, we also find that the fuzzer is able to build up on a
seed that does not match the pre-conditions and manages to generate
tests.
  $ ( ulimit -t 20; seacoral --tools libfuzzer )
  [A]{Sc} Starting to log into `_sc/tritype.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching libfuzzer on `tritype'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Simplified coverage results for `tritype':
  [A]{Sc} - Tests were generated
  [A]{Sc} Skipped reporting of lreplay results
