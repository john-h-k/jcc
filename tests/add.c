// expected value: 16

int puts(const char *);
int main() {
  // goto *&&x;

  // [[gnu::assume( ({x: puts("hi")}) )]]
  return 9 + 7;
}
