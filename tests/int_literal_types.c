// expected value: 69

int main() {
  int a = 0;
  int b = 00;
  int c = 0000;
  int d = 0x0;

  int e = 0xFE;
  int f = 010;
  int g = 0077;

  return a + b + c + d + e + f + g;
}
