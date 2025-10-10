int charcheck (char c) {
  if (c == '\000') return 0;
  if (c == '\001') return 1;
  if (c == '\042') return 2;
  if (c ==    'a') return 3;
  if (c ==    'Z') return 4;
  if (c == '\xff') return 5;
  return -1;
}
