/* The following program fails because foo is called with the wrong argument types*/

int foo(int a, int* b)
{
    return a;
}

int main()
{
    int v;
    int*w;
    v = foo(3,w);
    v = foo(2,3);
    return;
}