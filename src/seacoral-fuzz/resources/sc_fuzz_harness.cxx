/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU General Public    */
/*  License version 3.                                                    */
/*                                                                        */
/**************************************************************************/

// Note: this file is not compiled into a standalone object file: it is instead
// to be **included** into fuzzer-specific harness implementations

#ifndef __SC_FUZZ_HARNESS_CXX__
#define __SC_FUZZ_HARNESS_CXX__

#include "sc-raw-decoder.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>

// Extra include in case some C types used by the tested function involve _Bool
#include <stdbool.h>

// ---

// Shortcut back into LLVMFuzzerTestOneInput in case of sc_assume(0)
static jmp_buf on_sc_assume_false;

extern "C" void sc_assume (int b) {
  if (!b) longjmp (on_sc_assume_false, 1);
}

#define __SC_FUZZ_CATCH_ASSUME_ZERO(code)	\
  do {						\
    if (setjmp (on_sc_assume_false) == 0)	\
      code;					\
  } while (0)

// ---

#ifdef __SC_FUZZ_HAVE_SHARED_STORE
extern "C" unsigned int __sc_buff_commit (void);
# define __SC_FUZZ_COMMIT() (void) __sc_buff_commit ()
#else  /* no shared store: use an empty block */
# define __SC_FUZZ_COMMIT() do {} while (0)
#endif

#endif	// __SC_FUZZ_HARNESS_CXX__
