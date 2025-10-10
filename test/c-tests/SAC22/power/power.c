/* Returns X to the power of N, when N >= 0.
   Only 2 tests needed to cover all branches/decisions.
   Each path corresponds to exactly one value of N.
*/
#include<seacoral/annots.h>

int power(int X, int N) {
  sc_assume(N <= 100);
	int S = 1;
	int Y = X;
	int P = N;
	
	while(P >= 1) {
		if(P % 2 != 0) {
			P = P - 1;
			S = S * Y;
		}
		Y = Y * Y;
		P = P/2;
	}
	return S;
}
