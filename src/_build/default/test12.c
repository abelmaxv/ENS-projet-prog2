/* Test pour les chaines de caractères : trouver une lettre */

int main()
{
    int result;
    int counter; 
    int *s;

    result = 0;
    s = "ew";
    counter = 0;

    while (counter < 2)
    {
        if (*(s) == 'w')
        {
            result = 1;
        }
        counter++;
        s++;
    }
    return result;
}  