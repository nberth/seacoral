#include <math.h>

double obfuscated_one(float x) {
  double c = acos(x);
  double s = asin(x);
  double res = c * c + s * s; 
  if (res != 1.) return 0.;
  return res;
}
