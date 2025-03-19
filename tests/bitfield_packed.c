// skip: not yet implemented
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

  if (sizeof(f) != 1) {
    return 1;
  }

  unsigned *c = &f;
  c[0] |= 0xFFFFFF80;

  if (f.a != 1) {
    return 2;
  }

  if (f.b != 3) {
    return 3;
  }

  if (f.c != 0) {
    return 4;
  }
}

