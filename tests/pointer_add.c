// arch: arm64
// expected value: 21

// disabled on rv32i because `long long`

int main() {
  int a = 0;
  int *b = (int *)0;
  long *c = (long *)0;
  long long *d = (long long *)0;

  a += 1;
  b += 1;
  c += 1;
  d += 1;

  return a + (unsigned long long)b + (unsigned long long)c +
         (unsigned long long)d;
}
