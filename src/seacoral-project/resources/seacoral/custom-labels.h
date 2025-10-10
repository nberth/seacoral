/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU General Public    */
/*  License version 3.                                                    */
/*                                                                        */
/**************************************************************************/

#ifndef __CUSTOM_LABELS_H__
#define __CUSTOM_LABELS_H__

/* Be sure to start counting custom labels by 1 instead of 0 (__COUNTER__ is a
   preprocessor variable that is automatically incremented each time it is
   expanded). */
static unsigned char __counter_base_increment = __COUNTER__;

#define sc_label(cond) pc_label(cond, __COUNTER__, "Custom")

#endif	/* __CUSTOM_LABELS_H__ */
