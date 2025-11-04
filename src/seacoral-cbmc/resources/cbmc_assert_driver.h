/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU Affero General    */
/*  Public License.                                                       */
/*                                                                        */
/**************************************************************************/

#include <cbmc_annots.h>

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)

#define sc_cov_label_declare(id)		\
  static char __sc_cbmc_covered_##id = '\000'

#define pc_label(expr,id,...)			\
  do {						\
    if (expr)					\
      __sc_cbmc_covered_##id = '\001';		\
  } while (0)

#define pc_label_bindings(expr, ...) do {} while (0)
#define pc_label_sequence(expr, ...) do {} while (0)
#define pc_label_sequence_condition(expr, ...) do {} while (0)

#define sc_cover_statement(id, ...)			\
  do {							\
    __CPROVER_assert(!__sc_cbmc_covered_##id,		\
		     "__sc_assertion." TOSTRING(id));	\
  } while (0)
