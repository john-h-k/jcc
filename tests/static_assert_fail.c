// no-compile

int main() {
  static_assert(sizeof(char) != 1);
  static_assert(sizeof(char) != 1, "this test should fail");
  static_assert(1.0 * 2 != 2.0, "floats work");
}

