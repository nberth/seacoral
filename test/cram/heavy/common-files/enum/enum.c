
enum nat {ZERO, ONE, TWO, MORE};

enum nat f(enum nat u){
  if (u == ZERO) {
    return ONE;
  } else if (u == ONE) {
    return TWO;
  } else {
    return MORE;
  }
}
