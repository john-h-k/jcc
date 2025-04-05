// no-compile
// flags: -Werror

[[jcc::__format__(printf, 1, 2), gnu::format(printf, 1, 2)]] int printf(const char *, ...);

int main() {
  printf("%d\n", 1);
}

