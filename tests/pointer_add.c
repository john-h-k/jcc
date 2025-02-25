// expected value: 0

// disabled on rv32i because `long long`

int main() {
  int a = 0;
  int *b = (int *)0;
  long *c = (long *)0;

  a += 1;
  b += 1;
  c += 1;

  if ((unsigned long)a != 1) {
    return 1;
  }

  if ((unsigned long)b != sizeof(*b)) {
    return 2;
  }

  if ((unsigned long)c != sizeof(*c)) {
    return 3;
  }
}
