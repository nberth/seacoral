#include<stdlib.h>
#include<stdio.h>

char* mmemcpy(char *dest, char *src, int len) {
  char *d = dest;
  if(src == NULL) {
    return dest;
  }
  const char *s = src;
  while(len--)
    *d++ = *s++;
  
  return dest;
}

