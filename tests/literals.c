// expected value: 99
// arch-skip: rv32i

// rv32i needs i64 support for this test (as `hi = 0xFFFFFFFF` compiles to `hi = (int)0xFFFFFFFFl`)

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

  int hi = 0xFFFFFFFF;
  if (hi != -1) {
    return 1;
  }

  return c + a + b0 + b1 + c0 + c1 + d0 + d1 + d2 + d3;
}
