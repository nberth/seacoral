// Exemple used in the ICST'14 paper on labels
// Preconditions are mandatory

#include <stdio.h>
#include <seacoral/annots.h>
/***
 * Calcula el peso relativo de la bola que le pasamos como par�metro a relativeWeight
 * Fourballs(int, int, int, int).relativeWeight(int)
 * Nos sobran los mutantes que afecten a las otras operaciones
 */

int mA, mB, mC, mD;
int ccual;

int relativeWeight(int cual) {
  sc_assume((mA>0) & (mB>0) & (mC>0) & (mD>0));
  ccual=cual;
  int r[3];

  if (cual == 1) {

    r[0] = mA / mB;
    r[1] = mA / mC;
    r[2] = mA / mD;
  } else if (cual == 2) {

    r[0] = mB / mA;
    r[1] = mB / mC;
    r[2] = mB / mD;
  } else if (cual == 3) {

    r[0] = mC / mA;
    r[1] = mC / mB;
    r[2] = mC / mD;
  } else {

    r[0] = mD / mA;
    r[1] = mD / mB;
    r[2] = mD / mC;
  }
  return 0;
}
