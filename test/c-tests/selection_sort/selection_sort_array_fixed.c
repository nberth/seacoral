// a version with fixed-length arrays of 5

int n = 5;

void swap(int t[5], int i, int j){
  int tmp = t[i];
  t[i] = t[j];
  t[j] = tmp;
}

void sort(int t[5]){
  int i, j;
  int mi, mv;

  if(n <= 0)
    return;

  for(i = 0; i < n-1; i++) {
    mv = t[i];
    mi = i;
    for(j = i+1; j < n; j++) {
      if(t[j] < mv) {
	mi = j;
	mv = t[j];
      }
    }
    swap(t,i,mi);
  }
}
