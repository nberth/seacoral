#include "seacoral/custom-labels.h"

int f (int i){
  if (i == 0) {
    sc_label(i != 1);
    return 0;
  }
  sc_label(i == 0);
  if (i == 0) {
    sc_label(i == 0);
    return 1;
  }
  return -1;
}
