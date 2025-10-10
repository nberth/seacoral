/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU General Public    */
/*  License version 3.                                                    */
/*                                                                        */
/**************************************************************************/

/* To be directly included in test harness implementations.

   The following macros need to be defined:

   - __SC_PTR_DECODE(ptr_code, lb, ub, depth)

   - __SC_CONSTRAINED_CARRAY_LENGTH(ptr_code, size_ptr, ub, depth)

   - __SC_CSTRING_DECODE (ptr_code, depth)
 */

#ifndef __SC_RAW_DECODER_C__
#define __SC_RAW_DECODER_C__

#include "sc-raw-decoder.h"
#include <stdlib.h>	      /* malloc */
#include <obstack.h>	      /* we use an obstack to hold pointers to allocated
				 memory */

#ifdef SC_RAW_DECODER_DEBUG
# include <stdio.h>
# define log(args...) fprintf (stderr, args)
#else
# define log(args...)
#endif

#define obstack_chunk_alloc malloc
#define obstack_chunk_free free
static struct obstack obstack;	// device for growing [ptrs] array
static void **ptrs = NULL;	// growing array of malloc'ed pointers
static unsigned ptrs_count = 0;	// size of ptrs

void
__sc_decoder_init (void) {
  obstack_init (&obstack);
}

/* Free every currently allocated block of memory. */
void
__sc_decoder_reset (void) {
  unsigned i;		       /* NB: could reversing the `free`s make sense? */
  for (i = 0; i < ptrs_count; i++) {
    free (ptrs[i]);
  }
  ptrs = NULL;
  ptrs_count = 0;
}

#define register_allocated_pointer(ptr)					\
  do {									\
    if (ptrs == NULL) {							\
      ptrs = (void**)obstack_alloc (&obstack, sizeof (void*));		\
    } else {								\
      obstack_blank (&obstack, sizeof (void*));				\
    }									\
    ptrs[ptrs_count++] = ptr; /* assign to `ptrs[ptrs_count]`, and then \
				 increment index */			\
  } while (0)

/* --- */

/* Allocates `size` bytes, initialized from `buff`. */
static void*
__sc_malloc_n_copy (__sc_buff_t * const buff, const size_t size) {
  void *ptr = malloc (size);
  register_allocated_pointer (ptr);

  memcpy (ptr, buff->from, size);
  buff->from += size;

  return ptr;
}

/* Allocates `size+1` bytes, terminated with a `0`; initializes the leading
   `size` bytes from `buff`. */
static char*
__sc_malloc_n_copy_with_trailing_0 (__sc_buff_t * const buff,
				    const size_t size) {
  char * ptr = malloc (size + 1);
  register_allocated_pointer (ptr);

  memcpy (ptr, buff->from, size);
  buff->from += size;

  ptr[size] = '\000';

  return ptr;
}

void*
__sc_raw_decode_array (__sc_buff_t * const buff,
		       const long ptr_code,
		       const unsigned int lb,
		       const unsigned int ub,
		       const unsigned int depth,
		       const size_t item_size,
		       const ptr_init ptr_init) {
  void *array = NULL;
  long decoded_len =
    (long)__SC_PTR_DECODE (ptr_code, lb, ub, depth);

  if (decoded_len >= 0l) {
    unsigned int array_len = (unsigned int) decoded_len, i;

    /* TODO: deal with alignment? */
    if ((buff->remaining_bytes -= item_size * array_len) < 0l)
      return NULL;

    array = __sc_malloc_n_copy (buff, item_size * array_len);

    if (ptr_init != NULL) {
      unsigned long *p = (unsigned long*)array;
      for (i = 0; i < array_len; i++) {
	if ((ptr_init (buff, depth + 1, (void*) p) == NULL) &&
	    buff->remaining_bytes < 0l) {
	  array = NULL;
	  break;
	}
        p = (unsigned long*) ((unsigned char*)p + item_size);
      }
    }
  }

  return array;
}

void*
__sc_raw_decode_constrained_array (__sc_buff_t * const buff,
				   const unsigned long ptr_code,
				   unsigned long long * const size,
				   const unsigned int ub,
				   const unsigned int depth,
				   const size_t item_size,
				   const ptr_init ptr_init) {
  void *array = NULL;
  long long decoded_len =
    (long long)__SC_CONSTRAINED_CARRAY_LENGTH (ptr_code, *size, ub, depth);

  if (decoded_len >= 0L) {
    unsigned int array_len = (unsigned int) decoded_len, i;

    /* TODO: deal with alignment? */
    if ((buff->remaining_bytes -= item_size * array_len) < 0l)
      return NULL;

    *size = array_len;

    array = __sc_malloc_n_copy (buff, item_size * array_len);

    if (ptr_init != NULL) {
      unsigned long *p = (unsigned long*)array;
      for (i = 0; i < array_len; i++) {
	if ((ptr_init (buff, depth + 1, (void*) p) == NULL) &&
	    buff->remaining_bytes < 0l) {
	  array = NULL;
	  break;
	}
        p = (unsigned long*) ((unsigned char*)p + item_size);
      }
    }
  }

  return array;
}

char*
__sc_raw_decode_cstring (__sc_buff_t * const buff,
			 const unsigned long ptr_code,
			 const unsigned int depth) {
  char *str = NULL;
  unsigned int buff_len;

  buff_len = (unsigned int)(__SC_CSTRING_DECODE (ptr_code, depth));
  if (buff_len != 0u) {

    if ((buff->remaining_bytes -= buff_len - 1) < 0l)
      return NULL;

    /* Allocates buff_len bytes with a trailing 0, and copies buff_len-1 bytes
       from the buffer: */
    str = __sc_malloc_n_copy_with_trailing_0 (buff, buff_len - 1);
  }

  return str;
}

#endif	/* __SC_RAW_DECODER_C__ */
