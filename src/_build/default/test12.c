/* Test pour les chaines de caractères : trouver une lettre */

int main()
{
    int counter; 
    int *s;

    counter = 0;
    s = "w";

    while (counter < 1)
    {
        if (*(s) == 'w')
        {
            return 1;
        }
        counter++;
        s++;
    }
    return 0;
}  