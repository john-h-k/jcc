// expected value: 50

struct baz {
  int val;
};

struct baz get() {
  return (struct baz){10};
}

struct baz glb = {1};

void foo(void) {
  glb.val++;
}

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

  glb.val = 0;
  int z = 0;
  (a) ? z : (foo(), 0);
  if (glb.val) {
    return 4;
  }
  (a) ? (foo(), 0) : z;
  if (glb.val != 1) {
    return 5;
  }

  // gnu extension ternary
  int z1 = 0 ?: 1;
  if (z1 != 1) {
    return 6;
  }

  z1 = 2 ?: 1;
  if (z1 != 2) {
    return 8;
  }

  return a ? a ? a ? 50 : 51 : 52 : 53;
}
