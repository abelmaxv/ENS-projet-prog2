/* This program should fail because to argument variables have the same name */

int main(int a, int* a)
{
    return a;
}