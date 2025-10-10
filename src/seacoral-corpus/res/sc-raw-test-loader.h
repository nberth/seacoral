/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU General Public    */
/*  License version 3.                                                    */
/*                                                                        */
/**************************************************************************/

#ifndef __SC_RAW_TEST_LOADER_H__
#define __SC_RAW_TEST_LOADER_H__

#include <stddef.h>		/* size_t */
#include <stdbool.h>		/* bool */

struct raw_test { unsigned char * data; size_t size; bool allocated; };

void __sc_read_raw_test_from_stdin (struct raw_test * const raw_test,
				    const size_t max_size);

void __sc_load_raw_test_file (const char * const filename,
			      struct raw_test * const raw_test);

void __sc_load_raw_test_string (const char * const string,
				const size_t string_size,
				struct raw_test * const raw_test);

void __sc_unload_raw_test (struct raw_test * const raw_test);

#endif	/* __SC_RAW_TEST_LOADER_H__ */
