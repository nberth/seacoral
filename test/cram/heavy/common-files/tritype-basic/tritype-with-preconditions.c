/* Should return the type of the triangle
   which has sides of these lengths.
   3 = not a triangle
   2 = equilateral triangle
   1 = isoceles triangle
   0 = other triangle
*/
#include "seacoral/annots.h"

int tritype(int i, int j, int k){
	int type_code;
	sc_assume((0 <= i) && (i <= 100));
	sc_assume((0 <= j) && (j <= 100));
	sc_assume((0 <= k) && (k <= 100));
	
	if ((i == 0) || (j == 0) || (k == 0)) type_code = 4;
	else {
		type_code = 0;
		if (i == j) type_code = type_code + 1;
		if (i == k) type_code = type_code + 2;
		if (j == k) type_code = type_code + 3;
		if (type_code == 0){
			if ((i+j <= k) || (j+k <= i) || (i+k <= j))
				type_code = 4;
			else
				type_code = 1;
		}
		else if (type_code > 3) type_code = 3;
		else if ((type_code == 1) && (i+j > k)) type_code = 2;
		else if ((type_code == 2) && (i+k > j)) type_code = 2;
		else if ((type_code == 3) && (j+k > i)) type_code = 2;
		else type_code = 4;
	}
	return type_code;
}
