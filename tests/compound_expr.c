// expected value: 20
int main() {
  int a = 1;
  unsigned b = 2;
  // a, b;
  a = a + 8, b = a + b;
  return a + b;
}
