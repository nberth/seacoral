#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

unsigned long length (const char *str) {
  if (str == NULL)
    return 0ul;
  return strlen (str);
}

int query (const char *str) {
  if (str == NULL)
    return -1;
  if (strncmp (str, "foo", 4) == 0)
    return 1;
  else
    return 0;
}

int query_foo (const char *str) {
  unsigned long len = 0ul;
  const char *s = str;
  if (s != NULL)
    for (; *s; len++, s++);
  if (len == 3ul) {
    if (strncmp (str, "foo", 4) == 0)
      return 2;
    return 1;
  }
  return 0;
}
