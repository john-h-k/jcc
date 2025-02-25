struct x{
    int y;
};

int f()
{
    struct x z;
    z.y=13;
    return z.y;
}