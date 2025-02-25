int r1(int n);

int r2(int n)
{
    if(n==0){
        return 1;
    }else{
        return r1(n-1)+r1(n-1);
    }
}


int main()
{
    return !(r1(5)==32);
}
