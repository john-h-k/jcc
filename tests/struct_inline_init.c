// expected value: 7

struct bar {
  int a;
};

struct foo {
  int a;
  struct bar b;
};

int main() {
  struct foo f = {
    3, 4
  };

  return f.a + f.b.a;
}
