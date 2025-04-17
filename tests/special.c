// expected value: 8
// stdout: special.c 7 10:04:33 Dec 10 2025

int printf(const char *, ...);

int main() {
  printf("%s %d %s %s", __FILE__, __LINE__, __TIME__, __DATE__);
  return __LINE__;
}
