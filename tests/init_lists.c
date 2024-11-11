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
  // int a[2] = {};
  // int b[2] = {7};
  // int c[2] = {4, 8};

  // if (a[0] != 0) {
  //   return 1;
  // }
  // if (a[1] != 0) {
  //   return 2;
  // }

  // if (b[0] != 7) {
  //   return 3;
  // }
  // if (b[1] != 0) {
  //   return 4;
  // }

  // if (c[0] != 4) {
  //   return 5;
  // }
  // if (c[1] != 8) {
  //   return 6;
  // }

  // struct one_elem one_init_zero = {};
  // struct one_elem one_init_one = {1};

  // struct three_elem three_init_zero = {};
  // struct three_elem three_init_one = {1};
  struct three_elem three_init_two = {1, 2};
  // struct three_elem three_init_three = {1, 2, 3};

  // if (one_init_zero.a != 0) {
  //   return 7;
  // }

  // if (one_init_one.a != 1) {
  //   return 8;
  // }

  // if (three_init_zero.a != 0) {
  //   return 9;
  // }

  // if (three_init_zero.b != 0) {
  //   return 10;
  // }

  // if (three_init_zero.c != 0) {
  //   return 11;
  // }

  // if (three_init_one.a != 1) {
  //   return 12;
  // }

  // if (three_init_one.b != 0) {
  //   return 13;
  // }

  // if (three_init_one.c != 0) {
  //   return 14;
  // }

  // if (three_init_two.a != 1) {
  //   return 15;
  // }

  if (three_init_two.b != 2) {
    return 16;
  }

  if (three_init_two.c != 0) {
    return 17;
  }

  // if (three_init_three.a != 1) {
  //   return 18;
  // }

  // if (three_init_three.b != 2) {
  //   return 19;
  // }

  // if (three_init_three.c != 3) {
  //   return 20;
  // }

  return 0;
}
