/* This program fails because foo is called with the wrong number of arguments */

int foo(int a, int b)
{
    return a;
}

int *main()
{
    return foo(1,2,3);
}