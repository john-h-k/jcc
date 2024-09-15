// Expected value: 0

int *const *volatile *foo(void *bar, int *const *volatile *baz) {
  return baz;
}

int main() {
  int a = 0;
  int *const b = &a;
  int *const *volatile c = &b;

  foo(0, &c);
}

