typedef int bool;
int f (bool b) {
  if (b) return 0;
  return 1;
}

int g (bool *arr, int n) {
  if (n > 0 && arr[0]) return 0;
  if (n > 1 && arr[1]) return 1;
  if (n > 2 && arr[2]) return 2;
  return -1;
}
