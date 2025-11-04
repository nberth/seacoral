/**************************************************************************/
/*                                                                        */
/*  Copyright (c) 2025 OCamlPro                                           */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  This file is distributed under the terms of the GNU Affero General    */
/*  Public License.                                                       */
/*                                                                        */
/**************************************************************************/

#include <stdlib.h> // Needed for malloc

// typ: type of the variable to initialize
// var: a pointer to the variable
#define CONST_INIT(typ, var) \
  do {						\
    typ nondet;					\
    *var = nondet;				\
  } while (0)

// if_size: the constant size of the current allocation (to have a constant allocation)
// size_var: the size variable
// ptr_var: the formal of the initializing function
// ptr_var_typ: the type of the value pointed by ptr_var_typ
// ptr_typ_size: the constant size of a single value of type ptr_var_typ
#define PTR_MALLOC(if_size, size_var, ptr_var, ptr_var_typ, ptr_typ_size) \
  do { \
    if (size_var == if_size)						\
      *ptr_var = (ptr_var_typ*)malloc(if_size * ptr_typ_size);		\
  } while(0)

// max_size (constant): the maximum size of the pointer
// ptr_typ: the type of the pointed value
// ptr_var: the pointer variable
// ptr_size_var: a pointer to the size (= NULL if no constraint on size)
// alloc_ptr_body: the code that allocates ptr_var given its_size (a sequence
//                 of PTR_MALLOC)

#define PTR_INIT_FUNCTION_BODY(max_size, ptr_typ, ptr_var,		\
			       ptr_size_var, alloc_ptr_body)		\
  int to_init_size;							\
  if (ptr_size_var == ( int* ) 0){					\
    int nondet_size;							\
    __CPROVER_assume (nondet_size >= 0 && nondet_size <= max_size);	\
    to_init_size = nondet_size;						\
  } else {								\
    to_init_size = *ptr_size_var;					\
  }									\
  alloc_ptr_body;							\
  return to_init_size;
