// expected value: 65

struct foo {
  int a, b;
  long c;
};

int main() {
  struct foo foo;
  struct foo *p = &foo;
  p->a = 5;
  p->b = 10;
  foo.c = 50;

  return foo.a + foo.b + p->c;
}
