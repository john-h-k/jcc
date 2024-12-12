// expected value: 15

struct foo {
  int a;
  int b[4];
  int c;
};

int main() {
  struct foo f = { 0, 1, 2, 3, 4, 5, };

  return f.a + f.b[0] + f.b[1] + f.b[2] + f.b[3] + f.c;
}
