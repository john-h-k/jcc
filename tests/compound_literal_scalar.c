// expected value: 0

int main() {
  int a = (int){0};
  int *b = &(((int){10}));
  int c = {{{{{{{{{{{{20}}}}}}}}}}}};
  long d = (long){30};

  struct {
    long field;
  } e = {.field = (long){1}};

  if (a) {
    return 1;
  }

  if (*b != 10) {
    return 2;
  }

  if (c != 20) {
    return 3;
  }

  if (d != 30) {
    return 4;
  }

  if (e.field != 1) {
    return 5;
  }
}
