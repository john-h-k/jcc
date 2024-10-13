// expected value: 26

struct bar;
struct baz { struct baz* b; };
struct bat l;

struct foo {
  int a, b;
  char *c;
  unsigned long d[3];
};

struct bat { int a; };

int main() {
  l.a = 1;

  struct foo bar;
  bar.a = 25;
  return bar.a + l.a;
}
