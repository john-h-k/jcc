// expected value: 40

int main() {
  // should be parsed as `sizeof(int) * p`, not `sizeof((int) *p)`
  int p = 10;
  return sizeof (int) * p;
}
