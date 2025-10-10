#include <stddef.h>		/* NULL */

struct dumb_list {
  struct dumb_list *next;
};

int length (struct dumb_list *l) {
  if (l == NULL)
    return 0;
  else
    return length (l->next) + 1;
}
