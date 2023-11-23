// expected value: 10
int main() {
  int a = 10;
  int b = 0;

  while (a) {
    a = a - 1;
    b = b + 1;
  }

  return b;
}
