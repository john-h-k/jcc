// expected value: 3
int main() {
  int a = 1, b = 2;
  long c = a + b;

  unsigned char d = 0xFF;
  int e = d;

  if (e != 0xFF) {
    return 1;
  }
  
  return c;
}
