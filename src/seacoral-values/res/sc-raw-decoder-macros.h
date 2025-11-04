/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU Affero General    */
/*  Public License.                                                       */
/*                                                                        */
/**************************************************************************/

#ifndef __SC_RAW_DECODER_MACROS_H__
#define __SC_RAW_DECODER_MACROS_H__

#define __SC_RAW_DECODE_ARRAY(t, ptr_t, ap, lb, ub, init_func_addr)	\
  do {									\
    (ap) =								\
      (ptr_t) __sc_raw_decode_array (buff, (unsigned long)(ap), lb, ub, \
				     depth, sizeof (t),			\
				     init_func_addr);			\
    if ((ap) == NULL && buff->remaining_bytes < 0l)			\
      return NULL;							\
  } while (0)

#define __SC_RAW_DECODE_CONSTRAINED_ARRAY(t, ptr_t, ap, ub,		\
					  size_t, size_ptr,		\
					  init_func_addr)		\
  do {									\
    unsigned long long __s = (unsigned long long)(*(size_ptr));		\
    (ap) =								\
      (ptr_t) __sc_raw_decode_constrained_array (buff,			\
						 (unsigned long)(ap),	\
						 &__s, ub, depth,	\
						 sizeof (t),		\
						 init_func_addr);	\
    if ((ap) == NULL && buff->remaining_bytes < 0l)			\
      return NULL;							\
    if (__s != (unsigned long long)*(size_ptr))				\
      *(size_ptr) = (size_t)__s;					\
  } while (0)

#define __SC_RAW_DECODE_CSTRING(ap)					\
  do {									\
    (ap) = __sc_raw_decode_cstring (buff, (unsigned long)(ap) , depth);	\
    if ((ap) == NULL && buff->remaining_bytes < 0l)			\
      return NULL;							\
  } while (0)

#endif	/* __SC_RAW_DECODER_MACROS_H__ */
