/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU General Public    */
/*  License version 3.                                                    */
/*                                                                        */
/**************************************************************************/

//@ requires p;
void _sc_assume(int p) { }

#ifndef sc_assume
#define sc_assume(p, ...) _sc_assume(p)
#endif
