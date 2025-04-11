// expected value: 220
// arch: aarch64
// os: darwin

#include <stdarg.h>

int var(int num, ...) {
  va_list a;
  va_start(a, num);
  va_list b;
  va_copy(b, a);

  int sum = 0;
  for (int i = 0; i < num; i++) {
    int l = va_arg(a, int);
    int r = va_arg(b, int);

    sum += l + r;
  }

  return sum;
}

int main() {
  return var(5, 1, 2, 3, 4, 100);
}

