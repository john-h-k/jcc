// expected value: 0

__attribute__ ((noinline)) int printf(const char * , ...) __attribute__((__format__ (__printf__, 1, 2))) __attribute__((foo));

int foo __attribute__((bar));

int main() {
}
