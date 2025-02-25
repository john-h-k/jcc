
int f(int x, int y);

int main()
{
    if( (f(0,0)!=1) ) return 1;
    if( (f(0,1)!=1) ) return 1;
    if( (f(1,0)!=0) ) return 1;
    if( (f(-1,1)!=1) ) return 1;
    if( (f(1,-1)!=0) ) return 1;
    return 0;
}
