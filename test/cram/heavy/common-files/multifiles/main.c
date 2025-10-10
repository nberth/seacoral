#include "f.h"
#include "g.h"

int _main (int i){
  int x = f (i);
  int y = g (x);
  return (x == y);
}
