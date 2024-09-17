// Expected value: 5
// stdin: 3 4
// stdout: sqrt(3^2 + 4^2) = 5

int printf(const char *format, ...);
int scanf(const char *format, ...);

int main() {
  int a = 0, b = 0;
  scanf("%d %d", &a, &b);

  int r = a * a + b * b;

  int ans = 0;
  while (ans * ans != r) {
    ans = ans + 1;
  }

  printf("sqrt(%d^2 + %d^2) = %d\n", a, b, ans);
  return ans;
}
