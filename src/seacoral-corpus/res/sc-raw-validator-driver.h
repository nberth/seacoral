/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU Affero General    */
/*  Public License.                                                       */
/*                                                                        */
/**************************************************************************/

#ifndef __SC_VALIDATOR_DRIVER_H__
#define __SC_VALIDATOR_DRIVER_H__

/* #include "sc-raw-validator-log.h" (included on command-line) */
#include <stdlib.h>

#define __sc_log_label_id(id) __sc_log ("%u\n", id)

// Defined in store
extern unsigned char __sc_buff_covered (unsigned int id);

// The file in which we will store the labels covered by the given test.
FILE* labels_file;

// The labels covered by the test. Used to not register a label twice in
// [labels_file].
unsigned char* covered_buff;

// Initializes [labels_file] if "__SC_VALIDATOR_LABEL_FILE" is defined.
// Then defines [covered_buff].
void initFileAndBuff() {
  char* file = getenv("__SC_VALIDATOR_LABEL_FILE");
  if (file && !labels_file) labels_file = fopen(file, "w");
  if (!covered_buff) covered_buff = calloc(__SC_MAX_ID, sizeof(unsigned char));
  return;
}

// Closes labels_file & covered_buff properly. Must be called after any call
// to __sc_log.
void deinitFileAndBuff() {
  if (labels_file) { fclose(labels_file); labels_file = NULL; }
  if (covered_buff) { free(covered_buff); covered_buff = NULL; }
  return;
}

// Checks if a label already has been covered by the test.
unsigned char is_covered(unsigned int id) {
  return covered_buff[id - 1];
}

// Sets a label as covered.
void set_covered(unsigned int id) {
  covered_buff[id - 1] = '\001';
}

#ifndef __SC_VALIDATOR_IGNORE_LABELS

// When entering a label, initializes the buffer and file, if
// any specified. Then, checks in the store if the label is
// uncovered (and performs mandatory side effects) or in the
// local buffer (because we want duplicates) and, if so,
// marks it as covered in the local buffer.
# define pc_label(expr, id, ...)		\
  do {						\
    if (expr) {					\
      initFileAndBuff();			\
      if (!__sc_buff_covered (id)		\
	  || !is_covered (id)) {		\ 
        set_covered(id);			\
	/* log on first reach only */		\
	__sc_log_label_id (id);			\
      }						\
    }						\
  } while (0)

#else  /* ignore labels */

# define pc_label(expr, id, ...) __sc_log_label_id (id)

#endif

static inline void sc_assume (int b) {
  if (!b) {
    exit (__SC_ASSUMPTION_FAILURE_CODE);
  }
}

#endif	/* __SC_VALIDATOR_DRIVER_H__ */
