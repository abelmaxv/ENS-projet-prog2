/* The following program fails because we redeclare a global variable */

int global;

int main() {
  int* global;
  return;
}

