// expected value: 222

struct bytes {
  char a, b, c, d;
};

union bar {
  int value;
  struct bytes bytes;
};

struct foo {
  int a;
  long b;
  union bar c;
};

int main() {
  struct foo foo;
  foo.a = 50;
  foo.b = 100;
  foo.c.value = 57005;
  return foo.c.bytes.b;
}


