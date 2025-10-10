/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU General Public    */
/*  License version 3.                                                    */
/*                                                                        */
/**************************************************************************/

#ifndef __SC_VALIDATOR_DRIVER_H__
#define __SC_VALIDATOR_DRIVER_H__

#include <stdlib.h>
#include <stdio.h>

#ifndef __SC_VALIDATOR_IGNORE_LABELS

extern void __sc_buff_set_covered (unsigned int id);

# define pc_label(expr, id, ...)		\
  do {						\
    if (expr)					\
      __sc_buff_set_covered (id);		\
  } while (0)

#else  /* ignore labels */

# define pc_label(expr, id, ...)		\
  do {} while (0)

#endif

static inline void sc_assume (int b) {
  if (!b) {
    exit (__SC_ASSUMPTION_FAILURE_CODE);
  }
}

#endif	/* __SC_VALIDATOR_DRIVER_H__ */
