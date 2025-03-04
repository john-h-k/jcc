// expected value: 8

#define FOO \
\
\
(8)

#define FIZZ 1

int main() {
  int a = FOO;

#if !(FIZZ && FIZZ + 7 == 8)
  return 1;
#endif

#if (FIZZ ? 0 : 1)
  return 2;
#endif

#if FIZZ ? !FIZZ ? 1 : 0 : 1
  return 3;
#endif


#if BAR
 invalid c code;
#endif

  /* logic here is something */

  float f = 8.3E9;
  int k = 8777; // hello

  return a;
}
