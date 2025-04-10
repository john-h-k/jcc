// Expected value: 0
// stdout: Hello, World!

#include <stdio.h>
// int printf(const char *, ...);

int main() {
  int a = 1;
  int b = a + 2;

  printf("Hello, World!\n");
  int c = b;
  int k = c;
  return 0;
}
