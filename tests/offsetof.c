// expected value: 0

struct Foo {
  int a;
  int b;
};


long off_a = (long)&(((struct Foo *)0)->a);
long off_b = (long)&(((struct Foo *)0)->b);

int main() {
  if (off_a) {
    return 1;
  }

  if (off_b != sizeof(int)) {
    return 2;
  }
}
