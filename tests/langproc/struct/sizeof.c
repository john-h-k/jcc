struct x{
    int y;
};

int f()
{
    struct x y;
    return sizeof(y);
}