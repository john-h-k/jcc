// expected value: 20
int main() {
  int a = 1;
  unsigned b = 2;
  a, b;
  a = a + 8, b = a + b;

  int c = 1;
  int d = 1;
  int e = (d += 1, c = d, d);

  int f = 1;
  int *p = (f += 1, &f);

  if (f != 2) {
    return 4;
  }

  if (*p != 2) {
    return 5;
  }

  if (c != 2) {
    return 1;
  }

  if (d != 2) {
    return 2;
  }

  if (e != 2) {
    return 3;
  }

  return a + b;
}
