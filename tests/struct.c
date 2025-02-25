// expected value: 26

struct bar;
struct baz {
  struct baz *b;
};

struct foo {
  int a, b;
  char *c;
  unsigned long d[3];
};

struct bat {
  int a;
};

struct bat l;

int main() {
  l.a = 1;

  struct foo bar;
  bar.a = 25;
  return bar.a + l.a;
}
