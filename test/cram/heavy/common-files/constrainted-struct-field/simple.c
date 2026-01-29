struct s { int n; char *a; };

void simple (struct s s){
  if (s.n >= 2 && s.a[0] == 0) {
    s.a[1] ++;
  } else if (s.n >= 2 && s.a[1] == 0) {
    s.a[0] ++;
  } else if (s.n == 1) {
    s.a[0] --;
  }
}
