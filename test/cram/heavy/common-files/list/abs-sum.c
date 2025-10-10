#include <stddef.h>		/* NULL */

struct int_list {
  int elt;
  struct int_list *next;
};

int abs_sum (struct int_list *l) {
  if (l == NULL)
    return 0;
  else if (l->elt < 0)
    return (abs_sum (l->next) - l->elt);
  else
    return (abs_sum (l->next) + l->elt);
}
