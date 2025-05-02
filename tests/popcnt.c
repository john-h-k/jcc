// expected value: 0

int main() {
  int a = 7;

  if (__builtin_popcount(a) != 3) {
    return 1;
  }
}
