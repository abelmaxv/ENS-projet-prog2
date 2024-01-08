/*Exemple de fonction recursive : expodentiation rapide */

int exp_rap(int a, int n)
{
    int p;
    if (n == 0)
    {
        return 1;
    }
    else
    {
        p = exp_rap(a, n/2);
        if(n%2 == 0)
        {
            return p*p;
        }
        else
        {
            return p*p*a;
        }
    }
}

int main()
{
    return exp_rap(5,5);
}