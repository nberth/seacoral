Here we run the tools as normal, but also inspect Klee's harness to
check proper interpretation of pointer-handling parameters.

  $ seacoral --tools klee --config static.toml
  [A]{Sc} Starting to log into `_sc/static.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `static_'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `static_':
          cov: 4 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 3 tests
  [A]{Sc} Covered labels: {1, 2, 3, 4}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Covered
                 3: Covered
                 4: Covered
          Coverage: (4/4) 100.0%
  $ tail --lines +7 _sc/last/klee/harness.c
  int main () {
    short a[2];
    
    klee_make_symbolic (&a, sizeof (short[2]), "short[2]");
    
    (void) static_ (a);
    __SC_KLEE_EXIT ();
    return 0;
  }

  $ seacoral --tools klee --config dynamic.toml
  [A]{Sc} Starting to log into `_sc/dynamic.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `dynamic'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `dynamic':
          cov: 6 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 4 tests
  [A]{Sc} Covered labels: {1, 2, 3, 4, 5, 6}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Covered
                 3: Covered
                 4: Covered
                 5: Covered
                 6: Covered
          Coverage: (6/6) 100.0%
  $ tail --lines +7 _sc/last/klee/harness.c
  int main () {
    int n;
    short (* a);
    
    klee_make_symbolic (&n, sizeof (int), "int");
    klee_make_symbolic (&a, sizeof (short (*)), "short (*)");
    __SC_KLEE_INIT_CONSTRAINED_PTR_LEAF (&(a), int, &(n), 0, short);
  
    (void) dynamic (n, a);
    __SC_KLEE_EXIT ();
    return 0;
  }

  $ seacoral --tools klee --config dynamic_rev.toml
  [A]{Sc} Starting to log into `_sc/dynamic_rev.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `dynamic_rev'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `dynamic_rev':
          cov: 6 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 4 tests
  [A]{Sc} Covered labels: {1, 2, 3, 4, 5, 6}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Covered
                 3: Covered
                 4: Covered
                 5: Covered
                 6: Covered
          Coverage: (6/6) 100.0%
  $ tail --lines +7 _sc/last/klee/harness.c
  int main () {
    short (* a);
    int n;
    
    klee_make_symbolic (&a, sizeof (short (*)), "short (*)");
    klee_make_symbolic (&n, sizeof (int), "int");
    __SC_KLEE_INIT_CONSTRAINED_PTR_LEAF (&(a), int, &(n), 0, short);
  
    (void) dynamic_rev (a, n);
    __SC_KLEE_EXIT ();
    return 0;
  }

  $ seacoral --tools klee --config null-or-empty.toml
  [A]{Sc} Starting to log into `_sc/null-or-empty.c-DC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `null_or_empty'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `null_or_empty':
          cov: 4 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 3 tests
  [A]{Sc} Covered labels: {1, 2, 3, 4}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Covered
                 3: Covered
                 4: Covered
          Coverage: (4/4) 100.0%
  $ tail --lines +7 _sc/last/klee/harness.c
  int main () {
    unsigned char (* a);
    int n;
    
    klee_make_symbolic (&a, sizeof (unsigned char (*)), "unsigned char (*)");
    klee_make_symbolic (&n, sizeof (int), "int");
    __SC_KLEE_INIT_CONSTRAINED_PTR_LEAF (&(a), int, &(n), 0, unsigned char);
  
    (void) null_or_empty (a, n);
    __SC_KLEE_EXIT ();
    return 0;
  }

  $ seacoral --tools klee --config length5.toml
  [A]{Sc} Starting to log into `_sc/length5.c-CC-@1/logs/1.log'
  [A]{Sc} Initializing working environment...
  [A]{Sc} Doing the hard work...
  [A]{Sc} Launching klee on `length5'
  [A]{Sc} Extracting new testcases from corpus...
  [A]{Sc} Hard work done
  [A]{Sc} Coverage statistics for `length5':
          cov: 8 (100.0%) uncov: 0 (0.0%) unkwn: 0 (0.0%) with 4 tests
  [A]{Sc} Covered labels: {1, 2, 3, 4, 5, 6, 7, 8}
  [A]{Sc} Uncoverable labels: {}
  [A]{Sc} Crash statistics: rte: none
  [A]{Sc}        1: Covered
                 2: Covered
                 3: Covered
                 4: Covered
                 5: Covered
                 6: Covered
                 7: Covered
                 8: Covered
          Coverage: (8/8) 100.0%
