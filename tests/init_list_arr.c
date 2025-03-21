// expected value: 0

typedef long I;
typedef struct{I c[4];I b,e,k;} PT;

PT cases[] = {
  0, 1, 2, 3, 4, 5, 6,
  7, 8, 9, 10, 11, 12, 13
};

int main() {
  I* p = cases;

  for (int i = 0; i < 14; i++) {
    if (p[i] != i) {
      return i;
    }
  }
}
