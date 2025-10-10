// anonymous enumeration

typedef enum {ZERO, ONE, TWO, MORE} nat_t;

nat_t f(nat_t u){
  if (u == ZERO) {
    return ONE;
  } else if (u == ONE) {
    return TWO;
  } else {
    return MORE;
  }
}
