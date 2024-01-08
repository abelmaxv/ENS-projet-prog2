/* The following program fails because addition between pointers in not allowed */

int *main() {
  int* v;
  int* w;
  return v+w;
}

