/* Test pour les chaines de caractères : trouver une lettre */

int main()
{
    int counter; 
    int *s;

    counter = 0;
    s = "zefewezef";

    while (counter < 9)
    {
        if (*(s+(counter++)) == 'w')
        {
            return 1;
        }
    }
    return 0;
}  