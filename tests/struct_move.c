// expected value: 18

struct foo {
  int a;
  // int b[3];
};

int printf(const char *f, ...);

int main() {
  struct foo a;
  // a.b[0] = 1;
  // a.b[1] = 2;
  // a.b[2] = 3;

  a.a = 1;
  struct foo b = a;
  // b.b[0] = 2 * b.b[0];
  // b.b[1] = 2 * b.b[1];
  // b.b[2] = 2 * b.b[2];

  return b.a;
  // return a.b[0] + a.b[1] + a.b[2] + b.b[0] + b.b[1] + b.b[2];
}
