// expected value: 25

struct foo {
  int a, b;
  char *c;
  unsigned long d[3];
};

int main() {
  struct foo bar;
  bar.a = 25;
  return bar.a;
}
