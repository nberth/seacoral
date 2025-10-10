struct t {
  int integer;
  char letter;
};

void f (struct t u){
  if (u.integer == 0) {
    u.letter ++;
  } else {
    u.letter --;
  }
}
