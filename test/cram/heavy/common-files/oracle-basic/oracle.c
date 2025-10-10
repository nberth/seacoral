#include "types.h"
int oracle (const enum abc abc, const enum def def) {
  return (abc == A && def == D ||
	  abc == B && def == E ||
	  abc == C && def == F);
}
