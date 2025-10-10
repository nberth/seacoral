int length55 (int arr[static 5][5]){
  int res = 0;
  for (int i = 0; i < 5; i++) {
    for (int j = 0; i < 5; i++) {
      if (arr[i][j] == 1) res ++;
    }
  }
  if (res > 2) return 1; else return 0;
}
