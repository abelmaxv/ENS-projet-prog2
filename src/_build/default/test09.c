/* This program should fail : x++ is not an lvalue */


int main()
{
    int x;
    x++=3;
    return;
}