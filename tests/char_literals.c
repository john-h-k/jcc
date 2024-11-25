// expected value: 0
// stdout: -

int printf(const char *, ...);

int main() {
  char c = '\055';
  char d = '\u0060';

  printf("%c%c\n", c, d);
}
