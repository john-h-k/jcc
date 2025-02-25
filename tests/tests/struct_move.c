// expected value: 18

struct foo {
  int b[3];
};

int main() {
  struct foo a;
  a.b[0] = 1;
  a.b[1] = 2;
  a.b[2] = 3;


  struct foo b = a;
  b.b[0] = 2 * b.b[0];
  b.b[1] = 2 * b.b[1];
  b.b[2] = 2 * b.b[2];

  return a.b[0] + a.b[1] + a.b[2] + b.b[0] + b.b[1] + b.b[2];
}
