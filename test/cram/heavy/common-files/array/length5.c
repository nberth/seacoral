int length5(int* arr, int N){
  int res = 0;
  if (arr == 0) return -1;
  for (int i = 0; i < N; i++) {if (arr[i] == 1) res ++;}
  if (res > 5) return 1; else return 0;
}
