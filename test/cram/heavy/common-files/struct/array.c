struct t {
  int integer;
  char letter;
};

void f (struct t u[static 2]){
  if (u[0].integer == 0) {
    u[1].letter ++;
  } else if (u[1].integer == 1) {
    u[0].letter ++;
  }
}
