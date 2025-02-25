// expected value: 50

struct bar {
  int z;
};

struct foo {
  int a, b;
  char *c;
  unsigned long d[3];

  struct bar e;
};

int main() {
  struct foo bar;
  bar.a = 25;
  bar.e.z = 50;
  return bar.e.z;
}
