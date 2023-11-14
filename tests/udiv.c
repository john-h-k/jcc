int main() {
  int a = 1;
  unsigned b = 1;
  long c = 1;
  unsigned long d = 1;
  
  // a / b -> type `unsigned`
  // b / c -> type `long`
  return (a / b) + (b / c) + (c / d);
}
