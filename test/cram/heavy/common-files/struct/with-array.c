struct t {
  int integer[1];
  char letter[2];
};

void f (struct t u){
  if (u.integer[0] == 0) {
    u.letter[0] ++;
  } else {
    u.letter[1] ++;
  }
}
