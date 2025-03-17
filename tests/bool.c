// expected value: 0

#include <stdbool.h>

int main() {
  bool a = 10;

  if (!a) {
    return 1;
  }

  bool b = 100.0;

  if (!b) {
    return 2;
  }

  bool c = 0;

  if (c) {
    return 3;
  }

  bool d = 0.0;

  if (d) {
    return 4;
  }
}
