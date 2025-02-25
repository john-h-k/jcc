int f()
{
    int x;
    int *y=&x;
    *y=64;
    return x;
}
