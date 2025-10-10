#include <stddef.h>		/* NULL */

int f (short *a) {
  if (a == NULL) {
    return 1;
  }
  return 0;
}
