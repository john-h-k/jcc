// expected value: 0
// stdout: ~

int putchar(int);
int main() {
  char b[2] = { 0 };
  int N = 3;
  b[0] = ".,-~:;=!*#$@"[N > 0 ? N : 0];
  putchar(b[0]);
  putchar('\n');
}
