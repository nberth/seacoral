#include"seacoral/annots.h"

#define NULL ((char)0)
#define EOS 0
#define BASE_SZ 2

#define GET_CHAR(c,ret) { ( (indice>=100)? (ret) : (c = nondetEntry[indice++]) ) ; }

#define N 5

int loopcnt; 
#define CHKLOOP {if(++loopcnt >= N) return NULL;}

int ap_isspace(char c)
{
  if (c == '\t'
      || c == '\n'
      || c == '\v'
      || c == '\f'
      || c == '\r'
      || c == ' ')
    return 1;

  return 0;
}

int ap_tolower(char c)
{
  // do we have tolower() in our stubs?
  return c;
}

// Rewritten to be more analyzable -- use explicit array indexing. 
// char * ap_cpystrn(char *dst, const char *src, size_t dst_size)
char * ap_cpystrn(char *dst, const char *src, int dst_size)
{
  int i;

  if (dst_size == 0)
    return (dst);
  
  for (i = 0; i < dst_size - 1; i++) {
    dst[i] = src[i];
    if (src[i] == EOS) {
      return dst + i;
    }
  }

  dst[i] = EOS;

  return dst + i;
}


char *get_tag(char tag[static N], int tagbuf_len, char nondetEntry[static N])
{
  sc_assume(tagbuf_len > 0);
  sc_assume(tagbuf_len <= N);

  char *tag_val, c, term;
  int t, indice=0;
  loopcnt = 0;
  t = 0;

  --tagbuf_len;
	
  do {
    CHKLOOP
    GET_CHAR(c, NULL);
  } while (ap_isspace(c));

  if (c == '-') {
    GET_CHAR(c, NULL);
    if (c == '-') {
      do {
	CHKLOOP
        GET_CHAR(c, NULL);
      } while (ap_isspace(c));
      if (c == '>') {
        ap_cpystrn(tag, "done", tagbuf_len);
        return tag;
      }
    }
    return NULL;
  }

  while (1) {
    CHKLOOP
    if (t == tagbuf_len) {
      tag[t] = EOS;
      return NULL;
    }
    if (c == '=' || ap_isspace(c)) {
      break;
    }
    tag[t] = ap_tolower(c);
    t++;
    GET_CHAR(c, NULL);
  }

  tag[t] = EOS;
  t++;
  tag_val = tag + t;

  while (ap_isspace(c)) {
    CHKLOOP
    GET_CHAR(c, NULL);
  }
  if (c != '=') {
    return NULL;
  }

  do {
    CHKLOOP
    GET_CHAR(c, NULL);
  } while (ap_isspace(c));

  if (c != '"' && c != '\'') {
    return NULL;
  }
  term = c;
  while (1) {
    CHKLOOP
    GET_CHAR(c, NULL);
    if (t == tagbuf_len) { /* Suppose t == tagbuf_len - 1 */
      tag[t] = EOS;
      return NULL;
    }

    if (c == '\\') {
      tag[t] = c;
      t++;               /* Now t == tagbuf_len */
      GET_CHAR(c, NULL);
    }
    else if (c == term) {
      break;
    }
    tag[t] = c;    
    t++;                /* Now t == tagbuf_len + 1 
                         * So the bounds check (t == tagbuf_len) will fail */
  }
  tag[t] = EOS;

  return tag;
}
