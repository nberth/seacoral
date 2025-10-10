extern short a;
enum state { TESTABLE_1, TESTABLE_2 };
enum state tested_state;

void neg_init () {
  switch (tested_state) {
  case TESTABLE_1: default:
    a = 1;
    break;
  case TESTABLE_2:
    a = 2;
    break;
  }
}

int neg_oracle (int result) {
  return result < 0;
}
