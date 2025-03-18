// expected value: 0

int main() {
  static_assert(1.0 * 2 == 2.0, "floats work");
  static_assert(sizeof(char) == 1);
  static_assert(sizeof(int) == 4, "with message");
}
