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

// float sum_vec3(struct vec3 v) { return v.a + v.b + v.c; }

long sum_small(struct small s) {
  long sum = 0;
  for (int i = 0; i < 2; i++) {
    sum += s.buff[i];
  }

  return sum;
}

// long sum_big(struct big b) {
//   long sum = 0;
//   for (int i = 0; i < 16; i++) {
//     sum += b.buff[i];
//   }

//   return sum;
// }

int main() {
  // struct vec3 v = {.a = 1, .b = 2, .c = 3};
  struct small s = {{5, 7}};
  // struct big b = {{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16}};

  // if (sum_vec3(v) != 6) {
  //   return 1;
  // }

  if (sum_small(s) != 12) {
    return 1;
  }

  // if (sum_big(b) != 136) {
  //   return 1;
  // }
}
