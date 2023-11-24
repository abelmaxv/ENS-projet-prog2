/* This program should compile : to illustrate a 'complex' program that is suppose to work */

int global1;
int global2;

int* foo(int v, int* w)
{
    return &v-w;
}

int main(int a)
{
    int *v;
    if( global1 == 1 )
    {
        v = foo(a+3, v);
    }
    else
    {
        return a;
    }
    while (global2)
    {
        global2 --;
    }
    return *v;
}