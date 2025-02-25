int f()
{
    int i;
    int x[8];
    int acc;
    for(i=8; i<16; i++){
        x[i-8]=i;
    }
    acc=0;
    for(i=0; i<8; i++){
        acc=acc+x[i+0];
    }
    return acc;
}
