// expected value: 2

int foo() {
  static int a = 0;
  return a++;
}

int main() {
  foo();
  foo();
  return foo();
}
