/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU General Public    */
/*  License version 3.                                                    */
/*                                                                        */
/**************************************************************************/

#ifndef __SC_RAW_DECODER_H__
#define __SC_RAW_DECODER_H__

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct __sc_buff {
  const unsigned char *from;
  long remaining_bytes;
} __sc_buff_t;

/* --- */

/* Intialize the decoder of heap-allocated structures. */
void
__sc_decoder_init (void);

/* Reset the decoder's internal handling of heap-allocated structures.

   When `free_memory` is not zero, free every block of memory that has been
   heap-allocated (malloc'ed) since the last call to `__sc_decoder_init` or
   `__sc_decoder_reset`. */
void
__sc_decoder_reset (const unsigned free_memory);

typedef void *(*ptr_init) (__sc_buff_t * const,
			   const unsigned int,
			   void * const);

void*
__sc_raw_decode_array (__sc_buff_t * const buff,
		       const long ptr_code,
		       const unsigned int lb,
		       const unsigned int ub,
		       const unsigned int depth,
		       const size_t item_size,
		       const ptr_init ptr_init);

void*
__sc_raw_decode_constrained_array (__sc_buff_t * const buff,
				   const unsigned long ptr_code,
				   unsigned long long * const size,
				   const unsigned int ub,
				   const unsigned int depth,
				   const size_t item_size,
				   const ptr_init ptr_init);
  
char*
__sc_raw_decode_cstring (__sc_buff_t * const buff,
			 const unsigned long ptr_code,
			 const unsigned int depth);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif	/* __SC_RAW_DECODER_H__ */
