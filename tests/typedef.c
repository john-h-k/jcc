// expected value: 6

typedef int my_int;
typedef struct foo {
  int a;
} bar;

int main() {
  my_int a = 1;
  struct foo b = {2};
  bar c = {3};

  return a + b.a + c.a;
}
