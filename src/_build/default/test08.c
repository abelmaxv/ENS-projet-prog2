/* This program should fail because to argument variables have the same name */

int foo(int a, int* a)
{
    return a;
}

int main()
{
    int x;
    int *y;
    foo(x,y);
    return;
}