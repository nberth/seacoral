#include "const.h"
const int global_int = 42;

int foo (int x) {
  if (x > global_int)
    return 1;
  return 0;
}
