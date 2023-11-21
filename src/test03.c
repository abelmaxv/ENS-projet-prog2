/* The following program fails because we use a local variable of a bloc in another bloc*/

int foo(){
  int v;
  return v;
}

int main() {
  return v; 
}

