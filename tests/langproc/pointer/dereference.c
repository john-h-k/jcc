int f()
{
    int x;
    int *y=&x;
    x=13;    
    return *y;
}
