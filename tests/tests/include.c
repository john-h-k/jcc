// expected value: 66

#ifndef FOO
#define FOO
#include "include.c"

int main() {
  return foo;
}
#else
int foo = 66;
#endif
