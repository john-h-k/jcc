// expected value: 99
int main() {
  char c = 1;
  int a = 10;
  unsigned b0 = 11u;
  unsigned b1 = 11U;
  long c0 = 11l;
  long c1 = 11l;
  unsigned long d0 = 11ul;
  unsigned long d1 = 11Ul;
  unsigned long d2 = 11uL;
  unsigned long d3 = 11UL;
  return c + a + b0 + b1 + c0 + c1 + d0 + d1 + d2 + d3;
}
