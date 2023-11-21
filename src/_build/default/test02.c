/* The following program fails because addition between pointers in not allowed */

int *main(int a) {
  int* v;
  int* w;
  return v+w;
}

