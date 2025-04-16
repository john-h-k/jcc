// expected value: 0
// stdout: 0 5

int printf(const char *format, ...);

int *const *volatile *foo(void *bar, int *const *volatile *baz) {
  printf("%d %d", (int)bar, ***baz);
  return baz;
}

int main() {
  int a = 5;
  int *const b = &a;
  int *const *volatile c = &b;

  foo(0, &c);
}
