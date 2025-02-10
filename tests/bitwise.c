// expected value: 106

int main() {
  int a = 0xFE;
  int b = a & 1;
  int c = a | 0xFF;
  int d = a ^ 0xF;
  int e = ~a;
  int f = a << 1;
  int g = a >> 1;

  return a + b + c + d + e + f + g;
}
