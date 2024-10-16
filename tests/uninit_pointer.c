// expected value: 64

int main() {
  int a;

  int *p = &a;
  *p = 64;

  return (a + *p) / 2;
}
