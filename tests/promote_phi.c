// expected value: 10


int yes() { return 1; }

int main() {
  int a = 0;
  int *p = &a;

  if (yes()) {
    *p = 10;
  } else {
    *p = 50;
  }

  return *p;
}
