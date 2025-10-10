
union t {
  int entier;
  double flottant;
  char lettre;
};

void f(union t u){
  if (u.entier == 0) {
    u.flottant ++;
  } else {
    u.lettre --;
  }
}
