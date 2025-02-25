// expected value: 1

int main() {
  int a, b;
  float c = 1, d = 0;

  if (c) {
    a = 1;
  } else {
    a = 0;
  }

  if (d) {
    b = 1;
  } else {
    b = 0;
  }

  return a + (b << 1);
}
