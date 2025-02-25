// expected value: 0
// stdin: 10

int scanf(const char *format, ...);

int main() {
  int a;
  scanf("%d", &a);

  if (a == 0) {
    return 1;
  } else {
    return 10;
  }
}
