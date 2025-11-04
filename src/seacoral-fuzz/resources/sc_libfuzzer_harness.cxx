/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU Affero General    */
/*  Public License.                                                       */
/*                                                                        */
/**************************************************************************/

// Note: this file is not compiled into a standalone object file: it is instead
// to be **included** into a fuzzer-specific generated harness

#ifndef __SC_LIBFUZZER_HARNESS_CXX__
#define __SC_LIBFUZZER_HARNESS_CXX__

#include "sc_fuzz_harness.cxx"

/* Flag that indicates we are in merge mode.
   Unused for now. */
static unsigned char merge_mode = '\00';

extern "C" int LLVMFuzzerInitialize (const int *argc, const char ***argv) {
  int i;
  __sc_decoder_init ();
  /* Check whether "-merge_inner=1" belongs to the fuzzer's arguments (given to
     chils processes if the main call includes "-merge=1"), in which case the
     executable is used to merge seeds into the corpus.  Forcing crashes for
     seeds prevents libfuzzer from inserting inputs into the corpus solely on
     account of newly discovered features, instead of newly covered labels
     only. */
  for (i = *argc - 1;
       i > 0 && strcmp ("-merge_inner=1", (*argv)[i]) != 0;
       i--);
  merge_mode = i > 0 ? '\01' : '\00';
  return 0;
}

/* Label coverage flag (ignored if the hack below is disabled) */
unsigned char __sc_new_label_covered = '\00';

// ---

#ifndef __SC_FUZZ_ENABLE_LABELSONLY_HACK
# error "Please define __SC_FUZZ_ENABLE_LABELSONLY_HACK"
#endif
#if __SC_FUZZ_ENABLE_LABELSONLY_HACK==true

/* Libfuzzer hack to enforce label-only sensitivity (enabled: true) */
namespace fuzzer {
  class TracePC {
  public:
    void ClearInlineCounters();
  };
  void ClearExtraCounters ();
  extern TracePC TPC;
  static void __sc_clear () {
    TPC.ClearInlineCounters ();
    ClearExtraCounters ();
  }
}

# define __SC_FUZZ_LABELSONLY_REINIT()			\
  do { __sc_new_label_covered = '\00'; } while (0)
# define __SC_FUZZ_LABELSONLY_FILTER()		\
  do {						\
    if (!__sc_new_label_covered)		\
      fuzzer::__sc_clear ();			\
  } while (0)

#else

/* Uncomment to enable simplificiation in note (1) above.

   TODO: __SC_ASSUME_EXPRESSIONS_INDUCE_NO_BRANCHES */
// # define __SC_FUZZ_LABELSONLY_REINIT()			\
//   do { __sc_new_label_covered = '\00'; } while (0)
# define __SC_FUZZ_LABELSONLY_REINIT() do {} while (0)
# define __SC_FUZZ_LABELSONLY_FILTER() do {} while (0)

#endif

#endif	// __SC_LIBFUZZER_HARNESS_CXX__
