// expected value: 20
int main() {
  int a = 5;

  if (1) {
    a = 10;
  }

  int b = a + a;
  return b;
}
