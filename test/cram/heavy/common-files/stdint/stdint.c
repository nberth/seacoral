#include <stdint.h>
struct t { int16_t i; };
uint8_t f (struct t x) {
  if ((uint32_t)x.i >= (uint32_t)42) {
    return 0;
  } else {
    return 1;
  }
}
