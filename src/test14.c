int foo1(int x)
{
    int y;
    y = 3;
    return x + y;
}

int foo2(int x, int y)
{
    int a;
    int b;
    return foo1(x) + y;
}

int main()
{
    int a;
    int b;
    int c;
    return foo2(3,4);
}