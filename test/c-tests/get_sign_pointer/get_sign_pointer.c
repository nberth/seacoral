#include<stddef.h>
#include<stdio.h>

// 4 tests with PathCrawler
// 3 tests with KLEE because symbolic a cannot be NULL

int get_sign (int * a) {

    if (a == NULL)
		return 2;

    int x = *a;

    if (x == 0)
		return 0;

    if (x < 0)
		return -1;
	else
		return 1;

 }

int get_sign_struct(struct { int x; } *a) {

    if (a == NULL)
		return 2;

    int x = a->x;

    if (x == 0)
		return 0;

    if (x < 0)
		return -1;
	else
		return 1;

 }

int get_sign_struct_ptr(struct { int *x; } *a) {

    if (a == NULL || a->x == NULL)
		return 2;

    int x = *(a->x);

    if (x == 0)
		return 0;

    if (x < 0)
		return -1;
	else
		return 1;

 }
