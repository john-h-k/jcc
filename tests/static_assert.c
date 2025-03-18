#include <assert.h>

int main() {
  static_assert(sizeof(char) == 1);
  static_assert(sizeof(int) == 4, "with message");
}
