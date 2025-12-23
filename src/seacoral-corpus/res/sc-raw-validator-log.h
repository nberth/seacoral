/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU Affero General    */
/*  Public License.                                                       */
/*                                                                        */
/**************************************************************************/

#ifndef __SC_VALIDATOR_LOG_H__
#define __SC_VALIDATOR_LOG_H__

#include <stdlib.h>
#include <stdio.h>

// Defined in sc-raw-validator-driver.h
extern FILE* labels_file;

#if __SC_VALIDATOR_VERBOSITY > 0
# define __sc_pretty_log(msg...) do { fprintf (stderr, msg); } while (0)
#else
# define __sc_pretty_log(msg...) do {} while (0)
#endif

#define __sc_log(msg...)			  \
  do {						  \
    __sc_pretty_log(msg);			  \
    if (labels_file) fprintf(labels_file, msg);   \
  } while (0)

#endif	/* __SC_VALIDATOR_LOG_H__ */
