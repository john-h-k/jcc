// expected value: 10

struct foo {
  // int b[10];
  long b[2];
};

int main() {
  struct foo a;
  a.b[1] = 10;

  struct foo b = a;
  return b.b[1];
}
