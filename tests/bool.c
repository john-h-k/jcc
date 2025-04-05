// expected value: 0

#include <stdbool.h>

int main() {
  bool a = 10;

  if (!a) {
    return 1;
  }

  bool v = false;
  bool *p = &v;

  if (*p) {
    return 2;
  }

  *p = true;

  if (!*p) {
    return 3;
  }

  bool b = 100.0;

  if (!b) {
    return 4;
  }

  bool c = 0;

  if (c) {
    return 5;
  }

  bool d = 0.0;

  if (d) {
    return 6;
  }
}
