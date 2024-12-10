// expected value: 5

int main() {
  printf("%s %d %s %s", __FILE__, __LINE__, __TIME__, __DATE__);
  return __LINE__;
}
