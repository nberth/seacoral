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

/* #include "sc-raw-validator-log.h" (included on command-line) */
#include <stdlib.h>

#define __sc_log_label_id(id) __sc_log ("%u\n", id)

#ifndef __SC_VALIDATOR_IGNORE_LABELS

extern unsigned char __sc_buff_covered (unsigned int id);

# define pc_label(expr, id, ...)		\
  do {						\
    if (expr) {					\
      if (!__sc_buff_covered (id)) {		\
	/* log on first reach only */		\
	__sc_log_label_id (id);			\
      }						\
    }						\
  } while (0)

#else  /* ignore labels */

# define pc_label(expr, id, ...)		\
  __sc_log_label_id (id)

#endif

static inline void sc_assume (int b) {
  if (!b) {
    exit (__SC_ASSUMPTION_FAILURE_CODE);
  }
}

#endif	/* __SC_VALIDATOR_DRIVER_H__ */
