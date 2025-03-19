// expected value: 0

struct bar {
  const char *value;
};

struct foo {
  int a;

  struct bar b;
};

int main() {
  struct foo f = (struct foo){
    .a = 100,
    .b = (struct bar){
      .value = "hello"
    }
  };

  if (f.a != 100) {
    return 1;
  }

  if (f.b.value[0] != 'h') {
    return 2;
  }

  struct foo *p = &f;
  *p = (struct foo){
    .a = 200,
    .b = (struct bar){
      .value = "world"
    }
  };

  if (f.a != 200) {
    return 3;
  }

  if (f.b.value[0] != 'w') {
    return 4;
  }
}
