/* WM criterion triggers division by zero in a label. That's on purpose. */
int foo (int x) {
  if (1 + x)
    return 0;
  return 1;
}
