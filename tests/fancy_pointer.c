// expected value: 10

int main() {
  int a[4];
  int *p = a;

  *(p + 0) = 1;
  *(p + 1) = 2;
  *(p + 2) = 3;
  *(p + 3) = (p + 4) - (p);

  return p[0] + p[1] + p[2] + p[3];
}
