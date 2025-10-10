// Exemple used in the ICST'14 paper on labels
// I included all the dependencies in this file because sc does not include dependencies

#include<stdio.h>

#define EOS 0
#define BASE_SZ 2
#define LDAP_SZ 4

/* Size of the buffer being overflowed 
 * Must ensure that 0 < TOKEN_SZ - 1 */
#define TOKEN_SZ BASE_SZ + 1

/* This requires an explanation. escape_absolute_uri() gets passed a
 * buffer uri[] and an offset into uri[]. The loop which overflows
 * token[] is only executed if uri[] starts with the string LDAP of
 * size LDAP_SZ, and if the character in uri[] which is one past the
 * offset is a slash. Hence the LDAP_SZ (for the string LDAP) and the
 * first +1 (for the slash).
 *
 * The second +1 is because we increment our iterator over uri[] at
 * least once before reaching the loop which overflows token[].
 *
 * The TOKEN_SZ + 2 is there so that uri[] will have enough characters
 * after the offset to overflow token[].
 */
#define URI_SZ LDAP_SZ + 1 + 1 + TOKEN_SZ + 2
//-----------------------------------------

#include <seacoral/annots.h>

#define N 10
// must be at least 8 to reach maximum coverage

int loopcnt; 
#define CHKLOOP {if(++loopcnt >= 10) return;}

unsigned mystrlen(char *s) {
  int i;
  i = 0;
  while (s[i] != EOS)
    ++i;
  return i;
}

int mystrncmp (const char *s1, const char *s2, int n) {
  int i;
  int retval;
  i = 0;
  do {
    retval = s1[i] - s2[i];
    if (i >= n-1) return retval;
    if (retval != 0) return retval;
    if (s1[i] == EOS) return 0;
    i++;
  } while (1);
}


void escape_absolute_uri (char uri[static N], int scheme) {
  sc_assume(uri[N-1] == '\000');
  
  int cp;
  int c,i;
  loopcnt = 0;
  char LDAP[5]={"ldap"};
  
  char *token[TOKEN_SZ];

  if (scheme == 0
      || mystrlen(uri) < scheme) {
    return;
  }

  cp = scheme;

  if (uri[cp-1] == '/') {
    while (uri[cp] != EOS
           && uri[cp] != '/') {
      CHKLOOP /* Loop limit */
      ++cp;
    }

    if (uri[cp] == EOS || uri[cp+1] == EOS) return;
    ++cp;

    scheme = cp;

    if (mystrncmp(uri, LDAP, LDAP_SZ) == 0) {
      c = 0;
      token[0] = uri;

      while (uri[cp] != EOS
             && c < TOKEN_SZ-1) {
	CHKLOOP /* Loop limit */
        if (uri[cp] == '?') {
          ++c;
          token[c] = uri + cp + 1;
          uri[cp] = EOS;
        }
        ++cp;
      }
      
      return;
    }
  }

  return;
}
