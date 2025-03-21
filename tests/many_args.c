// expected value: 0

int foo(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k) {
  if (k != 11) {
    return 0;
  }

  return a + b + c + d + e + f + g + h + i + j + k;
}

struct byte16 {
  long a, b;
};

int bar(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, struct byte16 l, struct byte16 m) {
  if (k != 11) {
    return 0;
  }

  return a + b + c + d + e + f + g + h + i + j + k + l.a + l.b + m.a + m.b;
}

int main() {
  int res = foo(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);

  if (res != 66) {
    return res;
  }

  struct byte16 l = { 5, 10 };
  struct byte16 m = { 7, 11 };
  res = bar(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, l, m);

  if (res != 99) {
    return res;
  }
}
