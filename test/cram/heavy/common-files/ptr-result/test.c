#include <stdlib.h>

typedef enum {
  INT,
  FLOAT
} type_data;

typedef struct {
  int data;
  type_data type;
} value;

void add_value (value *left, value *right, value *result) {
  if(left != NULL && right != NULL)
    result->data = left->data + right->data;
}
