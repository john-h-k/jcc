// expected value: 254

int main() {
  int a = 77;
  int b = 25;

  int c = ++a;
  int d = a++;

  int e = ++a;
  int f = a++;

  int g = --b;
  int h = b--;

  int i = --b;
  int j = b--;

  return (a + b + c + d + e + f) | (g + h + i + j);
}
