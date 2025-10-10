#include <stdio.h>
/* check a string
 * don't check overlong sequences

 * 0 ok
 * 1 invalid multibyte sequence on the first byte
 * 2 invalid multibyte sequence on the second byte
 * 3 invalid multibyte sequence on the third byte
 * 4 invalid multibyte sequence on the fourth byte
 * 5 unfinished multibyte sequence
 * 6 invalid codepoint
*/

#include<seacoral/annots.h>

# define N 5

int check (char buffer[static N], int n) {
  sc_assume(n>=0);
  sc_assume(n<=N);
  
  int error = 0;

  for (int i = 0; error == 0 && i < n; i++) {

    unsigned char c = buffer[i];
    unsigned int codepoint = 0;    

    if (c < 128) {
		codepoint = c;
    } else if (c >= 192 && c < 224) { 
		codepoint = c - 192;

		if (i+1 < n) {
			c = buffer[++i];

			if (c >= 128 && c < 192) { 
				codepoint = codepoint * 64 + (c - 128);
			} else {
				error = 2;
			}
		} else {
			error = 5;
		}
    } else if (c >= 224 && c < 240) {
		codepoint = c - 224;
		if (i+2 < n) {
			c = buffer[++i];
			if (c >= 128 && c < 192) { 
				codepoint = codepoint * 64 + (c - 128);
				c = buffer[++i];
				if (c >= 128 && c < 192) {
					codepoint = codepoint * 64 + (c - 128);
				} else {
					error = 3;
				}
			} else {
				error = 2;
			}
		} else {
			error = 5;
		}
    } else if (c >= 240 && c < 248) {
		codepoint = c - 240;
		if (i+3 < n) {
			c = buffer[++i];
			if (c >= 128 && c < 192) {
				codepoint = codepoint * 64 + (c - 128);
				c = buffer[++i];
				if (c >= 128 && c < 192) {
					codepoint = codepoint * 64 + (c - 128);
					c = buffer[++i];
					if (c >= 128 && c < 192) {
						codepoint = codepoint * 64 + (c - 128);
					} else {
						error = 4;
					}
				} else {
					error = 3;
				}
			} else {
				error = 2;
			}
		} else {
			error = 5;
		}
    } else {
		error = 1;
    }

    if (codepoint > 0xD7FF && codepoint < 0xE000) {
		error = 6;
    }
  }
  return error;
}
