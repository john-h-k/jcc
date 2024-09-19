// expected value: 0

struct one_elem {
  int a;
};

struct three_elem {
  int a;
  long b;
  int c;
};

int main() {
  struct one_elem one_init_zero = {};
  struct one_elem one_init_one = { 1 };

  struct three_elem three_init_zero = {};
  struct three_elem three_init_one = { 1 };
  struct three_elem three_init_two = { 1, 2 };
  struct three_elem three_init_three = { 1, 2, 3 };

  if (one_init_zero.a != 0) {
    return 1;
  }

  if (one_init_one.a != 1) {
    return 1;
  }

  if (three_init_zero.a != 0) {
    return 1;
  }
  
  if (three_init_zero.b != 0) {
    return 1;
  }

  if (three_init_zero.c != 0) {
    return 1;
  }

  if (three_init_one.a != 1) {
    return 1;
  }

  if (three_init_one.b != 0) {
    return 1;
  }

  if (three_init_one.c != 0) {
    return 1;
  }

  if (three_init_two.a != 1) {
    return 1;
  }

  if (three_init_two.b != 2) {
    return 1;
  }

  if (three_init_two.c != 0) {
    return 1;
  }

  if (three_init_three.a != 1) {
    return 1;
  }

  if (three_init_three.b != 2) {
    return 1;
  }

  if (three_init_three.c != 3) {
    return 1;
  }

  return 0;
}
