// expected value: 103

struct bar {
  int a;
};

struct foo {
  int a;
  struct bar bar;
};

struct foo f = {
    .a = 10,
    .bar.a = 7,
};

struct foo z = {.a = 10,
                .bar = {
                    .a = 9,
                }};

int arr[10][10] = {
    [7][5] = 66,
};

struct bar b = {
    .a = 1,
};

int main() { return b.a + f.a + f.bar.a + arr[7][5] + z.a + z.bar.a; }
