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

#if __SC_VALIDATOR_VERBOSITY > 0
# define __sc_log(msg...) do { fprintf (stderr, msg); } while (0)
#else
# define __sc_log(msg...) do {} while (0)
#endif

#endif	/* __SC_VALIDATOR_LOG_H__ */
