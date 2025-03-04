// expected value: 8
// stdout: Hello, World! 10

#define FOO \
\
\
(8)

#define FIZZ 1

#define BAT

#define EMPTY(a, b, c)

int printf(const char *, ...);

#define PRINT(fmt, ...) printf(fmt, __VA_ARGS__)

int main() {
  EMPTY(hi, how are, you);

  int a = FOO;

  PRINT("%s %s %d\n", "Hello,", "World!", 10);

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

#if !defined(BAT) || !defined BAT
  return 4;
#elif defined(BAT) && defined BAT
  return a;
#else
  return 5;
#endif
}
