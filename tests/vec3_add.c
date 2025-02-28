// expected value: 0

struct vec3 {
  float a, b, c;
};

struct vec3 add(struct vec3 a, struct vec3 b) {
  a.a += b.a;
  a.b += b.b;
  a.c += b.c;
  return a;
}

struct vec3 add_tmp(struct vec3 a, struct vec3 b) {
  struct vec3 tmp;
  tmp.a = a.a + b.a;
  tmp.b = a.b + b.b;
  tmp.c = a.c + b.c;
  return tmp;
}

int main() {
  struct vec3 l = { 1, 2, 3, };
  struct vec3 r = { 0, 2, 2, };

  struct vec3 sum = add(l, r);

  if (sum.a != 1) {
    return 1;
  }

  if (sum.b != 4) {
    return 2;
  }

  if (sum.c != 5) {
    return 3;
  }

  sum = add_tmp(sum, l);

  if (sum.a != 2) {
    return 2;
  }

  if (sum.b != 6) {
    return 3;
  }

  if (sum.c != 8) {
    return 4;
  }
}
