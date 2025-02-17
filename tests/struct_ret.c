// expected value: 0

struct vec3 {
  float a, b, c;
};

struct small {
  long buff[2];
};

struct big {
  long buff[16];
};

struct vec3 mk_vec3(float a, float b, float c) {
  struct vec3 v = { a, b, c};
  return v;
}

struct small mk_small(long a, long b) {
  struct small s = {{ a, b }};
  return s;
}

struct big mk_big() {
  struct big b;

  for (int i = 0; i < 16; i++) {
    b.buff[i] = i;
  }

  return b;
}

int main() {
  struct vec3 v = mk_vec3(1, 2, 3);

  if (v.a != 1 | v.b != 2 | v.c != 3) {
    return 1;
  }

  struct small s = mk_small(5, 7);

  if (s.buff[0] != 5 || s.buff[1] != 7) {
    return 2;
  }

  // struct big b = mk_big();

  // for (int i = 0; i < 16; i++) {
  //   if (b.buff[i] != i) {
  //     return 1;
  //   }
  // }
}

