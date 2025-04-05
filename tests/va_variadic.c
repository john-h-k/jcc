// skip: not yet implemented
// expected value: 220

typedef __builtin_va_list va_list;

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 202000L
#define va_start(ap, ...) __builtin_va_start(ap, 0)
#else
#define va_start(ap, param) __builtin_va_start(ap, param)
#endif

#define va_copy(dest, src) __builtin_va_copy(dest, src)
#define va_end(ap) __builtin_va_end(ap)
#define va_arg(ap, type) __builtin_va_arg(ap, type)


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
