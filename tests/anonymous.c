// expected value: 13

struct foo {
  int a;

  int other;

  struct f {
    // this should not clash
    int b;
  };
  
  union {
    int b;
    int c;
  };
};

int main() {
  struct foo f = {
    .a = 1,
    .b = 2
  };

  int d = f.b;
  f.c = 10;

  f.other = 100;

  return f.a + d + f.b;
}
