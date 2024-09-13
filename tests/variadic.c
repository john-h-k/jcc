// expected value: 0
// stdout: "Hello, World!"

int printf(const char *format, ...);
int puts(const char *s);

int main() {
  printf("%s. Numbers work too - %d!", "Hello, World!", 2);
  puts("");
}


