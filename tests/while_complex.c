// expected value: 76
int main() {
  int a = 50, b = 2;
  int iter_count = 0;
  int c = 1;

  int diff = a - b;

  while (diff) {
    iter_count = iter_count + 1;
    a = a * c - 1;
    b = b * c + 1;

    diff = a - b;
  }

  return iter_count + a + b;
}
