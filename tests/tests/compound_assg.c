// expected value: 2

int main() {
  int a = 0;
  a += 15;
  a *= 4;
  a /= 4;
  a %= 10;
  a ^= 38459;
  a &= 256;
  a |= 3;
  a >>= 1;
  a <<= 1;
  return a;
}
