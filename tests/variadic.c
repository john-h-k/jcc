// expected value: 0
// stdout: "Hello, World!"

int printf(const char *format, ...);

int main() {
  printf("%s. Numbers work too - %d!\n", "Hello, World!", 2);
}

