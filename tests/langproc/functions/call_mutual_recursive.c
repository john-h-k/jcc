int r2(int n);

int r1(int n)
{
    if(n==0){
        return 1;
    }else{
        return r2(n-1)+r2(n-1);
    }
}
