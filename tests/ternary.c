// expected value: 7

struct baz {
  int val;
};

int main() {
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

  return a ? a ? a ? 7 : 8 : 9 : 10;
}
