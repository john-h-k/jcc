// expected value: 0

int main() {
  char a[1];
  char b[1 + 3];
  char c[1 ? 2 : 4];
  char d[1 * 5 + 9 + (~-1)];

  if (sizeof(a) != 1) {
    return 1;
  }

  if (sizeof(b) != 4) {
    return 2;
  }

  if (sizeof(c) != 2) {
    return 3;
  }

  if (sizeof(d) != 14) {
    return 4;
  }
}
