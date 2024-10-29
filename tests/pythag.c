// Expected value: 5
// stdin: 3 4
// stdout: sqrt(3^2 + 4^2) = 5

int printf(const char *format, ...);
int scanf(const char *format, ...);

int raise(int a);

int main() {
  long a = 0, b = 0;
  scanf("%ld %ld", &a, &b);

  long r = a * a + b * b;

  long ans = 0;
  while (ans * ans < r) {
    ans = ans + 1;
  }
  printf("ans: %ld\n", ans);

  if (ans * ans > r) {
    printf("%ld < sqrt(%ld^2 + %ld^2) < %ld\n", ans - 1, a, b, ans);
  } else {
    printf("sqrt(%ld^2 + %ld^2) = %ld\n", a, b, ans);
  }
  return ans;
}
