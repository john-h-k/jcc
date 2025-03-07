// expected value: 0

int read(int *p) {
  return *p;
}

int main() {
  int a = 7;

  if (10) {
    a = 1;
  } else {
    a = 80;
  }

  int *p = &a;

  if (0) {
    a += 100;
  } else {
    a *= 3;
  }

  return read(p) != 3;
}
