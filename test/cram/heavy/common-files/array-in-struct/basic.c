#include <stddef.h>

struct t { unsigned *p; };

unsigned foo (struct t s) {
  if (s.p == NULL) {
    return 0;
  } else {
    return s.p[0];
  }
}
