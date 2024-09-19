// expected value: 10

struct foo {
  int b[10];
};

int main() {
  // struct foo a;
  // a.b[5] = 10;

  // struct foo b = a;
  // return b.b[5];
  return 10;
}
