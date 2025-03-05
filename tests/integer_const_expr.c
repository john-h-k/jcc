// skip: not yet implemented
// expected value: 0

int main() {
  char a[1];
  char b[1 + 3];
  char c[1 ? 2 : 4];
  char d[1 * 5 + 9 + (~-1)]:

  return !(sizeof(a) == 1 & sizeof(b) == 4 & sizeof(c) == 2 & sizeof(d) ==
  14);
}
