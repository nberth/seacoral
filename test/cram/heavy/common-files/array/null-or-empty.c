#include <stddef.h>		/* NULL */
int null_or_empty (unsigned char a[], int n){
  if (a == NULL) {
    return 1;
  } else if (n == 0) {
    return 2;
  } else {
    return 0;
  }
}
