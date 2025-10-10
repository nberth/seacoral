int f (int *arr, int n) {
  if (n > 0 && arr[0]) return 0;
  if (n > 1 && arr[1]) return 1;
  return -1;
}
