// expected value: 0
// stdout: -`A

int printf(const char *, ...);

int main() {
  char c = '\055';
  char d = '\u0060';
  char e = '\x41';

  printf("%c%c%c\n", c, d, e);
}
