/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU General Public    */
/*  License version 3.                                                    */
/*                                                                        */
/**************************************************************************/

#ifndef __NOOP_LABELS_H__
#define __NOOP_LABELS_H__

#define pc_label(cond, id...) do { if (cond) {} } while (0)

#endif	/* __NOOP_LABELS_H__ */
