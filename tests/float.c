// expected value: 10
// stdout: 1.400000

int printf(const char *format, ...);

int main() {
  double a = 1.4;

  printf("%f\n", a);
  return 10;
}
