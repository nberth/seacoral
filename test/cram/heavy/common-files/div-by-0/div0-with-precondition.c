#include "seacoral/annots.h"
int div0 (int x, int y) {
  sc_assume (y == 0);
  if (x < 0) {
    return 0;
  } else {
    return x / y;
  }
}
