// Expected value: 0

int puts(const char *str);
void putchar(char c);
int getchar();

int main() {
  puts("Enter two sides of triangle as digits: ");

  int a = getchar() - 48;
  int b = getchar() - 48;

  puts("Side 1: ");
  putchar(a + 48);
  puts("Side 2: ");
  putchar(b + 48);

  int r = a * a + b * b;
  puts("Side 3 squared: ");
  while (r) {
    int c = r % 10;
    putchar(c + 48);
    r = r / 10;
  }
  return 0;
}
