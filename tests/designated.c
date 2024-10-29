// expected value: 18

struct bar { int a; const char *b; };

struct foo { int a; struct bar bar; };

struct foo f = {
  .a = 10,
  .bar.a = 7,
  .bar.b = "boo"
};

int arr[10][10] = {
  [7][5] = 66,
};

struct bar b = {
  .a = 1,
  .b = "hey"
};

int main() {
  return b.a + f.a + f.bar.a + arr[7][5];
}
