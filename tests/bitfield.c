// expected value: 6

struct foo {
  int a : 1;
  int b : 5;
  int c : 1;
};

int main() {
  struct foo f = {
    .a = 1,
    .b = 3,
    .c = 0
  };

  return f.a + f.b + f.c;
}
