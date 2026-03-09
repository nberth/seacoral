#include <math.h>

double obfuscated_one(float x) {
  double c = cos(x);
  double s = sin(x);
  double res = c * c + s * s; 
  if (res != 1.) return 0.;
  return res;
}
