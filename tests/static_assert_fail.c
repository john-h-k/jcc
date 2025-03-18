// no-compile

#include <assert.h>

int main() {
  static_assert(sizeof(char) != 1, "this test should fail");
}

