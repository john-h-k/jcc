int multiply(int x, int y)
{
    int acc=0;
    if(x < 0){
        return -multiply(-x, y);
    }
    
    while(x > 0){
        acc += y;
        x--;
    }
    return acc;
}
