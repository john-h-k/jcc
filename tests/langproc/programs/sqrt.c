int bsqrt(int lo, int hi, int val)
{
    while(lo+1 < hi){
        int mid=(lo+hi)>>1;
        int sqr=mid*mid;
        if(sqr <= val){
            lo=mid;
        }else{
            hi=mid;
        }
    }
    if( lo*lo < val ) {
        return hi;
    }else{
        return lo;
    }
}
