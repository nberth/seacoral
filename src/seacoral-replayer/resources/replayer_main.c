/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU General Public    */
/*  License version 3.                                                    */
/*                                                                        */
/**************************************************************************/

// Used by the replayer.

extern unsigned char __sc_new_label_covered;
extern unsigned int __sc_buff_commit (void);
extern int __sc_main (int argc, char* argv[]);

int main (int argc, char* argv[]) {
  __sc_main (argc, argv);
  if (__sc_new_label_covered != '\00') {
    (void) __sc_buff_commit ();
    return 0;
  } else {
    return 1;
  }
}
