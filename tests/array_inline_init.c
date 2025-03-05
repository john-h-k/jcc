// expected value: 0

struct foo {
  int a;
  int b[4];
  int c;
};

int main() {
  int a = { 7 };

  if (a != 7) {
    return 1;
  }

  // C23
  // TODO: zero expr impl
  // int b = { };
  // if (b != 0) {
  //   return 1;
  // }  

  struct foo f = { {0}, 1, 2, 3, 4, 5, };

  if (f.a != 0) {
    return 2;
  }

  if (f.b[0] != 1) {
    return 3;
  }

  if (f.b[1] != 2) {
    return 4;
  }

  if (f.b[2] != 3) {
    return 5;
  }

  if (f.b[3] != 4) {
    return 6;
  }

  if (f.c != 5) {
    return 7;
  }

  int md0[2][2] = { 0, 1, 2, 3 };
  int md1[2][2] = { {0, 1}, {2, 3} };

  for (int i = 0; i < 2; i++) {
    for (int j = 0; j < 2; j++) {
      if (md0[i][j] != (i * 2) + j) {
        return 8;
      }

      if (md1[i][j] != (i * 2) + j) {
        return 9;
      }
    }
  }

  return 0;
}
