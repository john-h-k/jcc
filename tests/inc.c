// expected value: 1

int main() {
  int a = 0;
  int b = a++;

  int c = 0;
  int d = ++c;

  return a == 1 && b == 0 && c == 1 && d == 1;
}
