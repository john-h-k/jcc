// expected value: 3

int main() {
  int a[4];

  int *p = &a[0];
  int *q = &a[3];

  return q - p;
}
