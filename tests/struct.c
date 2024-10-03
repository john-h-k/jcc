// expected value: 25

struct bar;
struct baz { struct baz* b; };
struct bat *l;

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
