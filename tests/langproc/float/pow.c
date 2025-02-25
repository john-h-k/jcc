float f(float x, int n)
{
    float acc=1.0f;
    int i=0;
    while(i<n){
        i++;
        acc=acc*x;
    }
    return acc;
}
