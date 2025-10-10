#include "g.h"

int f(int x) {
  if (x < 0)
    x = x + 1;
  else
    x = x - 1;
  return ( g(x) );
}
