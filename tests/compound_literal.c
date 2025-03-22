// expected value: 0

struct bar {
  const char *value;
};

struct foo {
  int a;

  struct bar b;

  int c;
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

  if (f.c) {
    return 3;
  }

  struct foo *p = &f;
  *p = (struct foo){
    .a = 200,
    .b = (struct bar){
      .value = "world"
    }
  };

  if (f.a != 200) {
    return 4;
  }

  if (f.b.value[0] != 'w') {
    return 5;
  }

  if (f.c) {
    return 6;
  }

  *p = (struct foo){
    .c = 1
  };

  if (f.a) {
    return 7;
  }

  if (f.b.value) {
    return 8;
  }

  if (f.c != 1) {
    return 9;
  }
}
