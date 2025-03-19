// expected value: 10
int main() {
  int a = 10;
  int b = 0;

  do {
    do {
      a = a - 1;
      b = b + 1;
    } while (0);
  } while (a);

  return b;
}
