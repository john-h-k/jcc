// expected value: 20
int main() {
  int a = 0;
  int c = 0;

  for (int b = 10; b; b = b - 1) {
    a = a + 2;
  }

  return a + c;
}
