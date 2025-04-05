// no-compile
// flags: -Werror

// [[jcc::__format__(printf, 1, 2), gnu::format(printf, 1, 2)]] int printf(const char *, ...);
int printf(const char *, ...) __attribute__((__format__(__printf__, 1, 2)));

int main() {
  printf("%s\n", 1);
  printf();
}

