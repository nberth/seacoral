void dynamic (int n, short a[]){
  if (n >= 2 && a[0] == 0) {
    a[1] ++;
  } else if (n >= 2 && a[1] == 0) {
    a[0] ++;
  } else if (n == 1) {
    a[0] --;
  }
}
