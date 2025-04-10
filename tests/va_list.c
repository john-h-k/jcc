// expected value: 220
// stdout: Hello, 10Hello, 10

typedef __builtin_va_list va_list;

#define va_start(ap, ...) __builtin_va_start(ap)

#define va_copy(dest, src) __builtin_va_copy(dest, src)
#define va_end(ap) __builtin_va_end(ap)
#define va_arg(ap, type) __builtin_va_arg(ap, type)


int vprintf(const char* restrict format, va_list vlist);

void var(const char *restrict format, ...) {
  va_list a;
  va_start(a, format);
  va_list b;
  va_copy(b, a);

  vprintf(format, a);
  vprintf(format, b);
}

int main() {
  var("%s, %d", "Hello", 10);
}
