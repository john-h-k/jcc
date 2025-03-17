// expected value: 0

struct bar {
  const char *value;
};

struct foo {
  int a;

  struct bar b;
};

int main() {
  // TODO: scalar compound literal
  // int a = (int){0};
  // int *b = &(((int){10}));

  struct foo f = (struct foo){
    .a = 100,
    .b = (struct bar){
      .value = "hello"
    }
  };

  // if (a) {
  //   return 1;
  // }

  // if (*b != 10) {
  //   return 2;
  // }

  if (f.a != 100) {
    return 3;
  }

  if (f.b.value[0] != 'h') {
    return 4;
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
