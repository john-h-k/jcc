// skip: stringify needs to prevent prescan
// expected value: 20
// stdout: Hello, World! 10

#ifndef PREPROC_C
#define PREPROC_C

#if !defined(__JCC__) || !defined(__jcc__) || !__JCC__ || !__jcc__
#error "__JCC__ and __jcc__ should be defined!"
#endif

#warning Hi test warning

#define FOO (20)

#define FIZZ 1

#define BAT

#define EMPTY(a, b, c)

#define GET_TRUE(a) 1
#define TRUE(a) GET_TRUE(a)

int printf(const char *, ...);

#define STR()

#define PRINT(fmt, ...) printf(fmt, ##__VA_ARGS__)
#define PRINT2(fmt, ...) printf(fmt __VA_OPT__(, ) __VA_ARGS__)

#define NOP(x)
#define NOP2(x) NOP(x)

#define EXP1(exp) exp
#define exp exp + 1

#define MKSTR(s) #s
#define foo 4
#if TRUE(1)
#if 1
int main() {
  NOP(hello);
  NOP2(Hello);

  if (EXP1(0)) {
    return 1;
  }

  EMPTY(hi, how are, you);

  int a = FOO;

  PRINT("");
  PRINT("%s %s %d\n", "Hello,", "World!", 10);

#if !(FIZZ && FIZZ + 7 == 8)
  return 2;
#endif

#if (FIZZ ? 0 : 1)
  return 3;
#endif

#if FIZZ ? !FIZZ ? 1 : 0 : 1
  return 4;
#endif

#if BAR
  invalid c code;
#endif

  const char *p = MKSTR(foo);
  if (p[0] != 'f') {
    return 5;
  }

  /* logic here is something */

  float f = 8.3E9;
  int k = 8777; // hello

#if !defined(BAT) || !defined BAT
  return 6;
#elif defined(BAT) && defined BAT
  return a;
#else
  return 7;
#endif
}
#endif

#endif
