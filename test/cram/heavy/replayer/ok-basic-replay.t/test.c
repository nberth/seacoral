
int global;

int test(int i, int j, int k)
{
  int __retres;

  if ((global || i == 0) || j == 0 && k == 0) {
    return -1;
  }
  return 1;
}


