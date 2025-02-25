
int f(int x, int y);

int main()
{
    return !(f(0xFFFF,0xFFFF00)==0xFF00);
}
