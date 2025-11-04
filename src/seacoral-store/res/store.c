/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU Affero General    */
/*  Public License.                                                       */
/*                                                                        */
/**************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/mman.h>

#ifdef SC_STORE_DEBUG
# define log(args...) fprintf (stderr, args)
#else
# define log(args...)
#endif

#define SC_STORE_MAP_FILE_ENVVAR "SC_STORE_MAP_FILE"
#define SC_STORE_TOOL_ID_ENVVAR "SC_STORE_TOOL_ID"
#define SC_STORE_TOOL_ID_DEFAULT 0x7f

#define SC_UNCOV_MASK 0x80
#define __sc_uncoverable_code(c) (((c) & SC_UNCOV_MASK) != 0)

static unsigned char tool_id = SC_STORE_TOOL_ID_DEFAULT;

/* File descriptor for shared memory array, kept around for locking purposes. */
static int fd;

/* When not null, `covered` holds a pointer to the byte *directly preceding* the
   shared memory array. */
static unsigned char* covered = NULL;

/* Loose upper bound for label identifier, determined based on the size of the
   mmapped file. */
static unsigned int max_id;

/* As in man pages */
#define handle_error(msg)					\
  do { perror(msg); exit(EXIT_FAILURE); } while (0)

void __attribute__((constructor)) __store_initialize () {
  const char* mapfile;
  const char* tool_id_str;
  struct stat s;

  log ("sc_store: initializing ;-)\n");

  if ((mapfile = getenv (SC_STORE_MAP_FILE_ENVVAR)) == NULL) {
    fprintf (stderr, "Environment variable " SC_STORE_MAP_FILE_ENVVAR
	     " not defined\n");
    exit (EXIT_FAILURE);
  }

  log ("sc_store: mmap(\"%s\"): ", mapfile);

  if ((fd = open (mapfile, O_RDWR)) == -1)
    handle_error ("open");

  if (fstat(fd, &s) == -1)
    handle_error ("fstat");

  if (s.st_size > 0l &&
      (covered = mmap (NULL, s.st_size,
		       PROT_READ | PROT_WRITE, MAP_SHARED,
		       fd, 0)) == MAP_FAILED)
    handle_error("mmap");

  log ("success\n");

  /* shift `covered` backward by one byte */
  covered -= 1;
  max_id = (unsigned int) s.st_size;

  /* Initialize tool identifier */
  if ((tool_id_str = getenv (SC_STORE_TOOL_ID_ENVVAR)) != NULL) {
    if (strlen (tool_id_str) != 1) {
      fprintf (stderr, "Tool identifier should concist of one character only"
	       " (got `%s')\n", tool_id_str);
    } else if (__sc_uncoverable_code (tool_id_str[0])) {
      fprintf (stderr, "Tool identifier byte should not have highest bit set"
	       " (got `%s')\n", tool_id_str);
    } else {
      tool_id = tool_id_str[0];
    }
  }
}

void __attribute__((destructor)) __store_finalize () {
  log ("sc_store: mumap(...): ");

  /* shift `covered` forward by one byte */
  if (max_id > 0 &&
      munmap (covered + 1, max_id) == -1)
    handle_error ("munmap");

  if (close (fd) != 0)
    handle_error ("close");

  log ("success\n");
  covered = NULL;
  fd = -1;
}

/* File locking facility */

#ifndef F_SETLKW
# error "Missing required advisory record locking feature (from `fcntl')!"
#endif

inline static
void flock_init (struct flock *l, unsigned int id) {
  l->l_whence = SEEK_SET;
  l->l_start = (int)id - 1;
  l->l_len = 1;
}

inline static
void flock_init_full (struct flock *l) {
  l->l_whence = SEEK_SET;
  l->l_start = 0;
  l->l_len = max_id;
}

inline static
void flock_doit (struct flock *l) {
  if (fcntl (fd, F_SETLKW, l) == -1)
    handle_error ("fcntl (F_SETLKW)");
}

inline static
void flock_start_reading (struct flock *l) {
  l->l_type = F_RDLCK;
  flock_doit (l);
}

inline static
void flock_start_writing (struct flock *l) {
  l->l_type = F_WRLCK;
  flock_doit (l);
}

inline static
void flock_release (struct flock *l) {
  l->l_type = F_UNLCK;
  flock_doit (l);
}

#define __check_id(id, code)						\
  do {									\
    assert ((id) > 0);							\
    if ((id) > max_id) {						\
      fprintf (stderr, "sc_store: id > max_id? "			\
	       "(id=%u, max_id=%u)\n", id, max_id);			\
      do code while (0);						\
    }									\
  } while (0)

#define __check_uncoverable(id, status)					\
  do {									\
    if (__sc_uncoverable_code (status)) {				\
      fprintf (stderr, "sc_store: warning: covering label %u "		\
	       "previously marked uncoverable (with `%c`)\n",		\
	       id, (status) & ~SC_UNCOV_MASK);				\
    }									\
  } while (0)

/* Actual exported function */

unsigned char __sc_covered (unsigned int id) {
  unsigned char res;
  struct flock l;

  __check_id (id, { return 0; });

  flock_init (&l, id);
  flock_start_reading (&l);
  res = covered[id];
  if (!res || __sc_uncoverable_code (res)) {
    flock_start_writing (&l);
    covered[id] = tool_id;
  }
  flock_release (&l);

  __check_uncoverable (id, res);

  return res;
}

unsigned char __sc_is_covered (unsigned int id) {
  unsigned char res;
  struct flock l;

  __check_id (id, { return 0; });

  flock_init (&l, id);
  flock_start_reading (&l);
  res = covered[id];
  flock_release (&l);

  return res;
}

void __sc_set_covered (unsigned int id) {
  unsigned char status;
  struct flock l;

  __check_id (id, { return; });

  flock_init (&l, id);
  flock_start_writing (&l);
  status = covered[id];
  if (!status || __sc_uncoverable_code (status)) {
    covered[id] = tool_id;
  }
  flock_release (&l);

  __check_uncoverable (id, status);
}

static unsigned char* covered_buff = NULL;

inline static
void buff_initialize_maybe (void) {
  struct flock l;

  if (covered_buff)
    return;

  if (! (covered_buff = malloc ((size_t) (max_id + 1))))
    handle_error ("calloc");

  flock_init_full (&l);
  flock_start_reading (&l);
  (void) memcpy (covered_buff + 1, covered + 1, (size_t) max_id);
  flock_release (&l);
}

void __sc_buff_set_covered (unsigned int id) {
  unsigned char status;

  __check_id (id, { return; });

  buff_initialize_maybe ();

  status = covered_buff[id];
  if (!status || __sc_uncoverable_code (status)) {
    covered_buff[id] = tool_id;
  }

  __check_uncoverable (id, status);
}

unsigned char __sc_buff_covered (unsigned int id) {
  unsigned char status;

  __check_id (id, { return '\00'; });

  buff_initialize_maybe ();

  status = covered_buff[id];
  if (!status || __sc_uncoverable_code (status)) {
    covered_buff[id] = tool_id;
  }

  __check_uncoverable (id, status);

  if (!status) {
    return __sc_is_covered (id); /* return up-to-date status */
  } else {
    return status;
  }
}

unsigned int __sc_buff_commit (void) {
  unsigned int id;
  unsigned int committed = 0;
  struct flock l;

  if (! covered_buff)		/* nothing to commit */
    return 0u;

  flock_init_full (&l);
  flock_start_writing (&l);
  for (id = 1; id <= max_id; id++) {
    unsigned char status = covered[id];
    if (!status && covered_buff[id]) {
      covered[id] = covered_buff[id];
      committed++;
    }
  }
  flock_release (&l);

  return committed;
}

void __sc_nop (void) {}
