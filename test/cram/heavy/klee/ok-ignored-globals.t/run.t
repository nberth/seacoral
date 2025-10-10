  $ seacoral --tools klee
  [A]{Sc} Starting to log into `_sc/ignored-global.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `f'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `f':
          cov: 2 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 2 tests
  [A]{Sc} Covered labels: {1, 2}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Covered
          Coverage: (2/2) 100.0%

Check that `__unused_global` is not symbolized:
  $ tail --lines +7 _sc/last/klee/harness.c
  int main () {
    int b;
    
    klee_make_symbolic (&b, sizeof (int), "int");
    
    (void) f (b);
    __SC_KLEE_EXIT ();
    return 0;
  }
