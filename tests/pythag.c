// Expected value: 25
// stdin: 34
// stdout: 52

int puts(const char *str);
void putchar(char c);
int getchar();

int main() {
  int a = getchar() - 48;
  int b = getchar() - 48;

  int r = a * a + b * b;
  int x = r;

  while (r) {
    int c = r % 10;
    putchar(c + 48);
    r = r / 10;
  }

  return x;
}
