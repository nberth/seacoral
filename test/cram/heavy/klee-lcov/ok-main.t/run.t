  $ seacoral --tools klee --lcov-report --lcov-text-report report.txt --no-lreplay
  [A]{Sc} Starting to log into `_sc/main.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `main'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `main':
          cov: 2 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 2 tests
  [A]{Sc} Covered labels: {1, 2}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc} Generated LCOV-report in `_sc/main.c-CC-@1/lcov/report'
  $ cat report.txt
      1|       |int var;
      2|      2|int main () {
      3|      2|  if (var == 0)
    ------------------
    |  Branch (3:7): [True: 50.00%, False: 50.00%]
    ------------------
      4|      1|    return -1;
      5|      1|  return 0;
      6|      2|}
  
