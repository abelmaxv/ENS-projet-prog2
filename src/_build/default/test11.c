/* Algorithme d'expodentiation rapide imperatif : calcul de a^n */

int a;
int n;

int main()
{
    int result;
    int y;

    a = 3; /* Values to modify for the test */
    n = 4;

    if (n<0)
    {
        result = 1;
    }
    else
    {
        y = 1;
        while (1<n)
        {
            if (n%2 == 1)
            {
                y = a*y;
                n = n-1;
            }
            a = a*a;
            n = n/2;
        }
        result = a*y;
    }
    return result;
}