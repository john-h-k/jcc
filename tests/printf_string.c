// expected value: 0
// stdout: hello world

int printf(const char *, ...);

int main() {
  const char *h = "hello";
  const char *w = "world";

  printf("%s %s\n", h, w);
}
