/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU General Public    */
/*  License version 3.                                                    */
/*                                                                        */
/**************************************************************************/

#ifndef __SC_KLEE_DRIVER_H__
#define __SC_KLEE_DRIVER_H__

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>		/* for __assert_fail */
#include <klee/klee.h>
#include "seacoral/annots.h"

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)

extern unsigned char __sc_is_covered (unsigned int id);
extern void __sc_set_covered (unsigned int id);
extern void __sc_buff_set_covered (unsigned int id);
extern unsigned int __sc_buff_commit (void);
extern void __sc_nop (void);

// Defines the temporary random variables.
// If in replay mode:
// - no random: when the condition is reached, take the path
// - label reachability: deletes the label file
// - after checking label coverage: continue to cover labels
// If not in replay mode:
// - random: path can be taken or not
// - label reachability: remove file, then assert false to stop klee
// - after checking label coverage: stop the execution

#ifdef KLEE_IGNORE_LABELS

# define pc_label(expr, id, ...) do {} while (0)

#elifdef KLEE_IN_NAIVE_MODE

# define pc_label(expr, id, ...)		\
  do {						\
    if (expr)					\
      /* __asm__ volatile ("") */__sc_nop ();	\
  } while (0)

#else  /* label-optimized mode */

# define NONDET(i) klee_int (TOSTRING (i))
# define pc_label(expr, id, ...)			\
  do {							\
    if (!NONDET (id)) {					\
      if (!__sc_is_covered (id)) {			\
	klee_assert (!(expr));				\
      }							\
      klee_silent_exit (0);				\
    }							\
  } while (0)

#endif	/* klee mode */

#ifdef KLEE_AVOID_BENIGN_TESTS
# define __SC_KLEE_EXIT() do { klee_silent_exit (0); } while (0)
#else
# define __SC_KLEE_EXIT() do {} while (0)
#endif

/* --- */

#define pc_label_bindings(expr, id, ...) do {} while (0)
#define pc_label_sequence(expr, id, ...) do {} while (0)
#define pc_label_sequence_condition(expr, id, ...) do {} while (0)

/* --- */

#ifndef __SC_KLEE_MAX_NON_NIL_PTR_DEPTH
# error __SC_KLEE_MAX_NON_NIL_PTR_DEPTH is not defined.
#endif

/* --- */

#define __SC_KLEE_CHECK(s, lb, ub, depth)				\
  klee_assume (((depth) >= __SC_KLEE_MAX_NON_NIL_PTR_DEPTH)		\
	       ? ((s) == 0L)						\
	       : (((s) <= 0L) || ((s) >= (lb) && (s) <= (ub))))

#define __SC_KLEE_ALLOC(ptr, s, item_type, init_block)			\
  do {									\
    if ((s) == 0L) {							\
      *(ptr) = NULL;							\
    } else if ((s) < 0L) {						\
      *(ptr) = malloc (0);						\
    } else {								\
      *(ptr) = malloc (sizeof (item_type) * s);				\
      klee_make_symbolic (*(ptr), sizeof (item_type) * s,		\
			  TOSTRING (item_type) "[_]");			\
      do init_block while (0);						\
    }									\
  } while (0)

#define __SC_KLEE_CHECK_CONSTRAINED(ptr, s, depth)			\
  do {									\
    if ((depth) >= __SC_KLEE_MAX_NON_NIL_PTR_DEPTH) {			\
      klee_assume ((unsigned long)*(ptr) == 0ul);			\
    }									\
  } while (0)

#define __SC_KLEE_ALLOC_CONSTRAINED(ptr, s, item_type, init_block)	\
  do {									\
    if ((unsigned long)*(ptr) == 0ul) {					\
      *(ptr) = NULL;							\
    } else if ((s) == 0ul) {						\
      *(ptr) = malloc (0);						\
    } else {								\
      *(ptr) = malloc (sizeof (item_type) * s);				\
      klee_make_symbolic (*(ptr), sizeof (item_type) * s,		\
			  TOSTRING (item_type) "[_]");			\
      do init_block while (0);						\
    }									\
  } while (0)

/* --- */

#define __SC_KLEE_INIT_PTR(ptr, lb, ub, depth, item_type, item_init)	\
  do {									\
    long __s = (long) *(ptr);						\
    __SC_KLEE_CHECK (__s, lb, ub, depth);				\
    __SC_KLEE_ALLOC (ptr, __s, item_type, {				\
	void* __p = *(ptr);						\
	for (long i = 0L; i < __s; i++, __p += sizeof (item_type)) {	\
	  item_init (__p, (depth) + 1);					\
	}								\
      });								\
  } while (0)
  
#define __SC_KLEE_INIT_PTR_LEAF(ptr, lb, ub, depth, item_type)		\
  do {									\
    long __s = (long) *(ptr);						\
    __SC_KLEE_CHECK (__s, lb, ub, depth);				\
    __SC_KLEE_ALLOC (ptr, __s, item_type, {});				\
  } while (0)

#define __SC_KLEE_INIT_CONSTRAINED_PTR(ptr, _size_typ, size, depth,	\
				       item_type, item_init)		\
  do {									\
    __SC_KLEE_CHECK_CONSTRAINED (ptr, (unsigned long)(*(size)), depth);	\
    __SC_KLEE_ALLOC_CONSTRAINED (ptr, (unsigned long)(*(size)), item_type, { \
	void* __p = *(ptr);						\
	for (long i = 0L; i < __s; i++, __p += sizeof (item_type)) {	\
	  item_init (__p, (depth) + 1);					\
	}								\
      });								\
  } while (0)

#define __SC_KLEE_INIT_CONSTRAINED_PTR_LEAF(ptr, _size_typ, size,	\
					    depth, item_type)		\
  do {									\
    __SC_KLEE_CHECK_CONSTRAINED (ptr, (unsigned long)(*(size)), depth);	\
    __SC_KLEE_ALLOC_CONSTRAINED (ptr, (unsigned long)(*(size)), item_type, {});	\
  } while (0)

/* --- */

#define __SC_KLEE_CSTRING_BUFF_SIZE			\
  sizeof (char[__SC_KLEE_MAX_CSTRING_LENGTH + 1])

#define __SC_KLEE_INIT_CSTRING(ptr, depth)				\
  do {									\
    unsigned long __s = (unsigned long) *(ptr);	    /* 1 for nonnull */ \
    klee_assume (__s >= 0L);						\
    klee_assume (((depth) >= __SC_KLEE_MAX_NON_NIL_PTR_DEPTH)		\
		 ? (__s == 0L)						\
		 : (__s <= 1L));					\
    if (__s <= 0L) {							\
      *(ptr) = NULL;							\
    } else {								\
      *(ptr) = malloc (__SC_KLEE_CSTRING_BUFF_SIZE);			\
      klee_make_symbolic (*(ptr), __SC_KLEE_CSTRING_BUFF_SIZE,		\
			  "cstring");					\
      klee_assume ((*(ptr))[__SC_KLEE_MAX_CSTRING_LENGTH] == '\000');	\
    }									\
  } while (0)

#endif	/* __SC_KLEE_DRIVER_H__ */
