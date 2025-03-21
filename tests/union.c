// expected value: 1

union foo {
  int a;
  int b;
};

int main() {
  union foo bar;
  bar.a = 1;
  return bar.b;
}
