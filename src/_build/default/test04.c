/* The following program fails because there isn't a return in every branch of the function */

int main()
{  
    int a;
    int b;
    if (a)
    {
        return b;
    }
    else
    {   
        b++;
    }
}