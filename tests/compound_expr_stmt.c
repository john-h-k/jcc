// expected value: 0

int printf(const char *, ...);

int main() {
  return ({
    printf("Hello!\n");
    0;
  });
}
