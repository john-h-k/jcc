// expected value: 0

struct agg {
  int a, b;
};

struct sub_init {
  struct agg agg;
  int c;
};

int main() {
  struct sub_init foo = {1, 2, 3};

  if (foo.agg.a != 1) {
    return 1;
  }

  if (foo.agg.b != 2) {
    return 2;
  }

  if (foo.c != 3) {
    return 3;
  }

  struct sub_init bar = {{4, 5}, 6};

  if (bar.agg.a != 4) {
    return 4;
  }

  if (bar.agg.b != 5) {
    return 5;
  }

  if (bar.c != 6) {
    return 6;
  }

  struct agg agg = {7, 8};
  struct sub_init baz = {agg, 9};

  if (baz.agg.a != 7) {
    return 7;
  }

  if (baz.agg.b != 8) {
    return 8;
  }

  if (baz.c != 9) {
    return 9;
  }
}
