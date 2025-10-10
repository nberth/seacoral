//#include<stdio.h>

/* coordonnees de l'angle superieur gauche
 * (dans un repere ou x augmente vers la droite
 *  et y augmente vers le bas)
 * largeur (vers la droite)
 * hauteur (vers le bas)
 */

/* invariants :
 * width > 0 && height > 0 &&
 * x + width < MAXINT && y + height < MAXINT
 */

typedef struct {
  int x;
  int y;
  int width;
  int height;
} rectangle;

/* renvoie vrai ssi A contient B
 */
int contains(rectangle A, rectangle B) {
  return (B.x >= A.x
	  && B.y >= A.y
	  && B.x + B.width <= A.x + A.width
	  && B.y + B.height <= A.y + A.height);
}

/* renvoie vrai ssi il existe des points
 * internes a la fois a A et a B
 * (les bords ne comptent pas pour la difference)
 */
int intersects(rectangle A, rectangle B) {
  return !((B.x + B.width <= A.x)
	   || (B.y + B.height <= A.y)
	   || (A.x + A.width <= B.x)
	   || (A.y + A.height <= B.y));
}

/* calcule la difference de A par B : A - B
 * renvoie au plus 4 rectangles, dont l'union est égale à A - B
 */
int difference(rectangle A, rectangle B, rectangle res[]) {
  int size = 0;

  if (!intersects(A,B) || contains(B,A)) {
    return size;
  }

  // calcule le rectangle superieur
  int raHeight = B.y - A.y;
  if (raHeight > 0) {
    rectangle top = {A.x, A.y, A.width, raHeight};
    res[size] = top;
    size++;
  }

  // calcule le rectangle inferieur
  int rbY = B.y + B.height;
  int rbHeight = A.height - (rbY - A.y);
  if (rbHeight > 0 && rbY < A.y + A.height) { // cette 2e condition ne peut jamais etre fausse
    rectangle bottom = {A.x, rbY, A.width, rbHeight};
    res[size] = bottom;
    size++;
  }

  int rectAHY = A.y + A.height;
  int y1 = B.y > A.y ? B.y : A.y;
  int y2 = rbY < rectAHY ? rbY : rectAHY;
  int rcHeight = y2 - y1;
  
  // calcule le rectangle gauche
  int rcWidth = B.x - A.x;
  if (rcWidth > 0 && rcHeight > 0) {
    rectangle left = {A.x, y1, rcWidth, rcHeight};
    res[size] = left;
    size++;
  }

  // calcule le rectangle droit
  int rbX = B.x + B.width;
  int rdWidth = A.width - (rbX - A.x);
  if (rdWidth > 0) {
    rectangle right = {rbX, y1, rdWidth, rcHeight};
    res[size] = right;
    size++;
  }
  
  return size;
}

/*
void main() {
  rectangle A = {0,6,5,5};
  rectangle B = {3,7,-2,7};
  rectangle C = {1,5,3,7};
  rectangle D = {1,7,2,3};

  printf("A contient B : %d\n", contains(A,B));
  printf("B contient A : %d\n", contains(B,A));
  printf("A et B ont une intersection : %d\n\n", intersects(A,B));

  rectangle res[4];
  int size = difference(A,B,res);
  int i;
  for(i = 0; i < size; i++) {
    rectangle r = res[i];
    printf("%d, %d, %d, %d\n", r.x, r.y, r.width, r.height);
  }

}
*/
