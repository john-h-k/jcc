// expected value: 13

struct foo {
  int a;
  union {
    int b;
    int c;
  };
};

int main() {
  return 13;
  // struct foo f = {
  //   .a = 1,
  //   .b = 2
  // };
  // struct foo f;
  // f.a = 1;
  // f.b = 2;

  // int d = f.b;
  // f.c = 10;

  // return f.a + d + f.b;
}