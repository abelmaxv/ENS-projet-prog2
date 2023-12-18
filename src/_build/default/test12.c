/* Test pour les chaines de caractères : trouver une lettre */

int main()
{
    int result;
    int mark;
    int counter; 
    int *s;

    result = 0;
    s = "ew";
    counter = 0;

    while (counter < 3)
    {
        if (*(s) == 'w')
        {
            result = 1;
            mark = counter;
        }
        counter++;
    }
    return result;
}  