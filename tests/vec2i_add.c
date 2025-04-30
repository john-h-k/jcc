// expected value: 0

struct int2 {
  int a, b;
};

int addi(int _0, int _1, int _2, int _3, int _4, int _5, int _6,
         struct int2 a) {
  return a.a + a.b;
}

struct vec2 {
  float a;
  int b;
};

struct vec2 add(struct vec2 a, struct vec2 b) {
  a.a += b.a;
  a.b += b.b;
  return a;
}

struct vec2 add_tmp(struct vec2 a, struct vec2 b) {
  struct vec2 tmp;
  tmp.a = a.a + b.a;
  tmp.b = a.b + b.b;
  return tmp;
}

int main() {
  struct vec2 l = {
      1,
      2,
  };
  struct vec2 r = {
      0,
      2,
  };

  struct vec2 sum = add(l, r);

  if (sum.a != 1) {
    return 1;
  }

  if (sum.b != 4) {
    return 2;
  }

  sum = add_tmp(sum, l);

  if (sum.a != 2) {
    return 2;
  }

  if (sum.b != 6) {
    return 3;
  }

  struct int2 ints = {11, 33};
  int isum = addi(0, 1, 2, 3, 4, 5, 6, ints);

  if (isum != 44) {
    return 4;
  }
}
