/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU Affero General    */
/*  Public License.                                                       */
/*                                                                        */
/**************************************************************************/

extern unsigned char __sc_buff_covered (unsigned int id);
unsigned char __sc_new_label_covered = '\00';

#define pc_label(expr,id,...)					\
  do {								\
    if (expr) {							\
      __sc_new_label_covered |= !__sc_buff_covered (id);	\
    }								\
  } while (0)
