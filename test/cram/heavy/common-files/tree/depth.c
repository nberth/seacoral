#include <stddef.h>		/* NULL */

struct int_tree {
  int elt;
  struct int_tree *left, *right;
};

int depth (struct int_tree *t) {
  if (t == NULL)
    return 0;
  else {
    int l = depth (t->left), r = depth (t->right);
    return 1 + (l > r ? l : r);
  }
}
