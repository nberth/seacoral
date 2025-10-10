#include <stddef.h>		/* NULL */
int foo (void* p) {
  if (p == NULL) return 1;
  return 0;
}
