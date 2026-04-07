#include <stddef.h>

struct cell { int value; };
struct s { int n; struct cell *a; };

void struct_ (struct s *s){
  if (s == NULL) return;
  if (s->n >= 2 && s->a[0].value == 0) {
    s->a[1].value ++;
  } else if (s->n >= 2 && s->a[1].value == 0) {
    s->a[0].value ++;
  } else if (s->n == 1) {
    s->a[0].value --;
  }
}
