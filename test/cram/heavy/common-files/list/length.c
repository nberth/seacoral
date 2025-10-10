#include <stddef.h>		/* NULL */

struct int_list {
  int elt;
  struct int_list *next;
};

int length (struct int_list *l) {
  if (l == NULL)
    return 0;
  else
    return length (l->next) + 1;
}
