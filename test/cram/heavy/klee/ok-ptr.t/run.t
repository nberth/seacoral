Here we run the tools as normal, but also inspect Klee's harness to
check proper interpretation of pointer-handling parameters.

  $ seacoral --tools klee --config default.toml
  [A]{Sc} Starting to log into `_sc/default.c-DC-@1/logs/1.log'
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
  $ tail --lines +7 _sc/last/klee/harness.c
  int main () {
    short (* a);
    
    klee_make_symbolic (&a, sizeof (short (*)), "short (*)");
    __SC_KLEE_INIT_PTR_LEAF (&(a), 0l, 1l, 0, short);
  
    (void) f (a);
    __SC_KLEE_EXIT ();
    return 0;
  }
