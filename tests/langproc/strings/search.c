char *search(char *x, char c)
{
    while(*x){
        if(*x==c){
            return x;
        }
        x=x+1;
    }
    return 0;
}
