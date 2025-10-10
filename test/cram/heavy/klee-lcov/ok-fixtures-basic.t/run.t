  $ seacoral --tools klee --files ok.c --lcov-report --lcov-text-report report.txt --no-lreplay
  [A]{Sc} Starting to log into `_sc/ok.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `neg'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `neg': cov: 0 uncov: 0 unkwn: 0 with 0 test
  [A]{Sc} Covered labels: {}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc} Oracle statistics: fails: none
  [A]{Sc} Generated LCOV-report in `_sc/ok.c-DC-@1/lcov/report'
  $ cat report.txt
      1|       |short a;
      2|      0|int neg () {
      3|      0|  return -a;			/* ok */
      4|      0|}
  
  $ seacoral --tools klee --files fail.c --lcov-report --lcov-text-report report.txt --no-lreplay
  [A]{Sc} Starting to log into `_sc/fail.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `neg'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `neg': cov: 0 uncov: 0 unkwn: 0 with 0 test
  [A]{Sc} Covered labels: {}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc} Oracle statistics: fails: 1 test
  [A]{Sc} Generated LCOV-report in `_sc/fail.c-DC-@1/lcov/report'
  $ cat report.txt
      1|       |short a;
      2|      0|int neg () {
      3|      0|  return a;			/* trivially wrong code */
      4|      0|}
  
  $ seacoral --tools klee --files uncov.c --lcov-report --lcov-text-report report.txt --no-lreplay
  [A]{Sc} Starting to log into `_sc/uncov.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `neg'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `neg':
          cov: 1 (50.0%) uncov: 0 (0.0%) unkwn: 1 (50.0%) with 1 test
  [A]{Sc} Covered labels: {2}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc} Oracle statistics: fails: none
  [A]{Sc} Generated LCOV-report in `_sc/uncov.c-DC-@1/lcov/report'
  $ cat report.txt
      1|       |short a;
      2|      1|int neg () {
      3|      1|  if (a == 0)
    ------------------
    |  Branch (3:7): [True: 0.00%, False: 100.00%]
    ------------------
      4|      0|    return 0;			/* uncoverable with given fixtures */
      5|      1|  else
      6|      1|    return -a;
      7|      1|}
  
