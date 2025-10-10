int f (int *arr) {
  int res = 0;
  if (arr == 0) return -1;
  for (int i = 0; i < 25; i++) {
    if (arr[i] == 1) res ++;
  }
  if (res > 2) return 1; else return 0;
}
