// expected value: 12

int main() {
  int a[2] = { 0 };

  int *p = &a[0];
  *p++ = 5;
  *p++ = 7;

  return a[0] + a[1];
}
