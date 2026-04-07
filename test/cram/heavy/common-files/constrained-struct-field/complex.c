#include <stddef.h>

struct s { int n; char *a; };

void complex (struct s *s){
  if (s == NULL) return;
  if (s->n >= 2 && s->a[0] == 0) {
    s->a[1] ++;
  } else if (s->n >= 2 && s->a[1] == 0) {
    s->a[0] ++;
  } else if (s->n == 1) {
    s->a[0] --;
  }
}
