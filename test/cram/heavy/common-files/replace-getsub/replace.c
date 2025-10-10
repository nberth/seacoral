/*  -*- Last-Edit:  Mon Dec  7 10:31:51 1992 by Tarak S. Goradia; -*- */


#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#include <stdbool.h>

//# define NULL 0

# define MAXSTR 100
# define MAXPAT MAXSTR

# define ENDSTR  '\0'
# define ESCAPE  '@'
# define CLOSURE '*'
# define BOL     '%'
# define EOL     '$'
# define ANY     '?'
# define CCL     '['
# define CCLEND  ']'
# define NEGATE  '^'
# define NCCL    '!'
# define LITCHAR 'c'
# define DITTO   -1
# define DASH    '-'

# define TAB     9
# define NEWLINE 10

# define CLOSIZE 1

typedef char character;
typedef char string[MAXSTR];


int addstr(char c, char *outset, int *j, int maxset){
	_Bool result;

	if (*j >= maxset){
		result = false;
	} else {
		outset[*j] = c;
		*j = *j + 1;
		result = true;
	}
	return result;
}

char esc(char *s, int *i){
	char result;

	if (s[*i] != ESCAPE){
	  result = s[*i];
	}else{
		if(s[*i + 1] == ENDSTR){
			result = ESCAPE;
		}
		else{
			*i = *i + 1;
			if(s[*i] == 'n'){
				result = NEWLINE;
			} else {
				if(s[*i] == 't'){
					result = TAB;
				} else{
					result = s[*i];
				}
			}
		}
	}
	return result;
}



int makesub(char* arg, int from, character delim, char* sub){
	int result;
	int	i, j;
	_Bool junk;
	character escjunk;

	j = 0;

	i = from;
	while ((arg[i] != delim) && (arg[i] != ENDSTR)) {
		if (arg[i] == (unsigned)('&')){
			junk = addstr(DITTO, sub, &j, MAXPAT);
		} else {
			escjunk = esc(arg, &i);
			junk = addstr(escjunk, sub, &j, MAXPAT);
		}
		i = i + 1;
	}

	if (arg[i] != delim){
		result = 0;
	}else {
		junk = addstr(ENDSTR, &(*sub), &j, MAXPAT);
		if (!junk){
			result = 0;
		}else{
			result = i;
		}
	}
	return result;
}

_Bool getsub(char* arg, char* sub){
	int	makeres;

	makeres = makesub(arg, 0, ENDSTR, sub);

	if(makeres > 0){
		return 1;
	}
	else{
		return 0;
	}
}

/*
int main() {

  _bool res;
  char arg[10] = {'a','b','c','d','e','\n','g','h','i','\0'};
  char sub[10];

  res = getsub(arg, sub);

}

*/
