// expected value: 0

struct foo {
  unsigned a : 1;
  // unsigned : 0;
  unsigned b : 5;
  // unsigned : 0;
  unsigned c : 1;
};

int main() {
  struct foo f = {
    .a = 1,
    .b = 3,
    .c = 0
  };

  if (f.a != 1) {
    return 1;
  }

  if (f.b != 3) {
    return 2;
  }

  if (f.c != 0) {
    return 3;
  }
}
