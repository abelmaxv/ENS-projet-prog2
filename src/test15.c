int c;


int foo()
{
    if (c=0)
    {
        return 0;
    }
    else
    {
        c--;
        return foo();
    }
}

int main()
{
    return foo();
}