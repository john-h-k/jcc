// expected value: 0

#define yes 10

#if defined no ? no >= 10L : defined yes
int main()
#else
#error fail
#endif


#if defined yes ? yes >= 10L : defined no
{
  return 0;
}
#else
#error fail
#endif
