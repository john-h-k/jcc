// expected value: 0
// stdout: foomain

int printf(const char *, ...);

void foo() {
  printf("%s", __func__);
}

int main() {
  foo();
  printf("%s\n", __func__);
}
