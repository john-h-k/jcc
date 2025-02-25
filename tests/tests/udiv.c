// expected value: 5
int main() {
  int a = 7;
  unsigned b = 2;
  long c = 5;
  unsigned long d = 2;

  // a / b -> type `unsigned`
  // b / c -> type `long`
  return (a / b) + (b / c) + (c / d);
}
