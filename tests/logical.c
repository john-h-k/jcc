// expected value: 7

int main() {
  int a = 1;

  int b = !a;

  int c = 0;
  int d = 0;

  if (!a && ++b) {
    c = 5;
  }

  if (a || ++b) {
    d = 7;
  }

  return b + c + d;
}
