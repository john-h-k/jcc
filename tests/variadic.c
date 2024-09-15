// expected value: 0
// stdout: Hello, World!. Numbers work too - 2!

int printf(const char *format, ...);

int main() { printf("%d %d Numbers work too - %d!", 3, 5, 2); }
