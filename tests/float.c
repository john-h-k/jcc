// expected value: 10
// stdout: 1.400000 1.400000 -1.400000 -1.400000

int printf(const char *format, ...);

int main() {
  double a = 1.4;

  printf("%f %f %f %f\n", a, 1.4, -a, -1.4);
  return 10;
}
