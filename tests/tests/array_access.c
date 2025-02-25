// expected value: 222

int main() {
  int a = 57005;
  char *b = (char *)&a;
  return b[1];
}
