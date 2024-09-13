// expected value: 0
// stdout: Hello, World!. Numbers work too - 2!

int printf(const char *format, ...);

int main() {
  printf("%s. Numbers work too - %d!", "Hello, World!", 2);
}


