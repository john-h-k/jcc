// expected value: 162

struct baz {
  int *p;
};

union bar {
  struct baz *a[1];
};

struct foo {
  int a, b;
  long c[7];
  union bar *bar;
};

int main() {
  int val = 97;
  struct baz baz;
  baz.p = &val;

  union bar bar;
  bar.a[0] = &baz;

  struct foo foo;
  struct foo *p = &foo;
  p->a = 5;
  p->b = 10;
  foo.c[5] = 50;
  foo.bar = &bar;

  return foo.a + foo.b + p->c[5] + p->bar->a[0]->p[0];
}
