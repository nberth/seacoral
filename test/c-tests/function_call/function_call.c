// function f calls function g

int g(int a) {
  if(a < 0) 
    return -1;
  
  if(a > 0)
    return 1;

  return 0;
}

int f(int a, int b) {
  if(g(a) >= 0 && g(b) < 0)
    return 1;

  return -1;
}
