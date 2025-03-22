// expected value: 7

struct baz {
  int val;
};

struct baz get() {
  return (struct baz){10};
}

struct baz glb = {1};

int main() {
  int lhs = 1;
  int rhs = 2;

  struct baz f;
  struct baz *p = &f;

  *p = lhs && rhs ? get() : glb;
  if (p->val != 10) {
    return 1;
  }

  int a = 1;

  struct baz l = {1};
  struct baz r = {2};

  struct baz v = 1 ? l : r;
  if (v.val != 1) {
    return 2;
  }

  v = 0 ? l : r;
  if (v.val != 2) {
    return 3;
  }

  int *p = (a ? &a : 0);
  if (*p != a) {
    return 100;
  }

  return a ? a ? a ? 7 : 8 : 9 : 10;
}
