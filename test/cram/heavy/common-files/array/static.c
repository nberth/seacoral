void static_ (short a[static 2]){
  if (a[0] == 0) {
    a[1] ++;
  } else if (a[1] == 0) {
    a[0] ++;
  }
}
