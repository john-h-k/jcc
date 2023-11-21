// expected value: 11
int main() {
  int a;

  if (50) {
    a = 10;
  } else {
    a = 20;
  }

  int b;

  if (0) {
    b = 5;
  } else {
    b = 1;
  }

  return a + b;
}
