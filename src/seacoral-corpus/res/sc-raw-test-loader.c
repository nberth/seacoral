/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU General Public    */
/*  License version 3.                                                    */
/*                                                                        */
/**************************************************************************/

#include <stdio.h>		/* perror */
#include <stdlib.h>		/* exit */
#include <stddef.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/mman.h>

#include "sc-raw-test-loader.h"

/* As in man pages */
#define handle_error(msg)					\
  do { perror(msg); exit(EXIT_FAILURE); } while (0)

void __sc_read_raw_test_from_stdin (struct raw_test * const raw_test,
				    const size_t max_size) {
  raw_test->data = malloc (max_size); /* assume ok */
  raw_test->allocated = true;

  if (raw_test->data == NULL)
    handle_error ("malloc");

  if ((raw_test->size = fread (raw_test->data, 1, max_size, stdin)) == 0)
    handle_error ("fread");
}

void __sc_load_raw_test_file (const char * const filename,
			      struct raw_test * const raw_test) {
  int fd;
  struct stat s;

  if ((fd = open (filename, O_RDWR)) == -1)
    handle_error ("open");

  if (fstat (fd, &s) == -1)
    handle_error ("fstat");

  if ((raw_test->data = mmap (NULL, s.st_size,
			      PROT_READ, MAP_PRIVATE,
			      fd, 0)) == MAP_FAILED)
    handle_error("mmap");

  if (close (fd) != 0)
    handle_error ("close");

  raw_test->size = s.st_size;
  raw_test->allocated = false;
}

void __sc_load_raw_test_string (const char * const string,
				const size_t string_size,
				struct raw_test * const raw_test) {
  raw_test->data = (void *) string;
  raw_test->size = string_size;
  raw_test->allocated = false;
}

/* Note: we don't call unmap as it's done automatically when the process
   terminates; we may need an `unload_test` if and when a pool of worker
   sub-processes is used for replay. */

void __sc_unload_raw_test (struct raw_test * const raw_test) {
  if (raw_test->allocated) {
    free (raw_test->data);
  }
  raw_test->data = NULL;
  raw_test->size = 0L;
  raw_test->allocated = false;
}
