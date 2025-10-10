// I have been receiving multiple requests for my algorithm of arbitrary
// matrix inversion using the same matrix storage memory. It has been already
// posted in this newsgroup. Now I repost it again with comment: special
// (e.g. tridiagonal) matrices, in order to reduce amount of calculations, may
// require just slightly changed algorithm of LU-factorization (it is well
// known).

 /* This Pascal procudure is inteneded to efficiently invert
 matrix using just memory for original matrix storage
 (C) A.V.Kharchenko */

#define MaxDim 25 // just for example

typedef double ArrrR1[MaxDim][MaxDim];

void invert(ArrrR1 A, int N) {
   double C, S;
   int i, j, k;

// ------------ A = L * U - factorization ----------
// -------- L[i,i] assumed to be equal to 1 -----

   for (i = 0; i < N; ++i) {
      S = 0.0;
      for (k = 0; k < i; ++k)
         S += A[i][k] * A[k][i];
      A[i][i] -= S; // ----- Beware of zero! ----
      for (j=i+1; j < N; ++j) {
         S = 0.0;
         C = 0.0;
         for (k=0; k < i; ++k) {
            S += A[i][k] * A[k][j];
            C += A[k][i] * A[j][k];
         };
         A[i][j] -= S;
         A[j][i] = (A[j][i] - C) / A[i][i]; // ------ !!! ------
      }
   };

// ------------- L-matrix inversion to <L> ---------

   for (j = 0; j < N-1; ++j) {
      for (i = j+1; i < N; ++i) {
         S = A[i][j];
         for (k = j+1; k < i; ++k)
            S += A[i][k] * A[k][j];
         A[i][j] = -S;
      };
   };

// ------------- U-matrix inversion to <U> ---------

   for (i = 0; i < N; ++i) {
      A[i][i] = 1.0/A[i][i];
      for (j = i+1; j < N; ++j) {
         S = 0;
         for (k = i; k < j; ++k)
            S = S + A[i][k] * A[k][j];
         A[i][j] = -S/A[j][j];
      };
   };

// ---------- Multiplication <A> = <U>*<L> ----------

   for (i = 0; i < N; ++i) {
      for (j = 0; j < N-1; ++j) {
         if (i > j) {
            S = 0.0;
            k = i;
         }
         else {
           S = A[i][j];
           k = j+1;
         };
         for (; k < N; ++k)
            S += A[i][k] * A[k][j];
         A[i][j] = S;
      }
   };
}

