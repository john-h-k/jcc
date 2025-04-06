// expected value: 0

struct foo bar();

struct foo {
  int a;
};

struct foo bar() {
  return (struct foo){1};
}

int main() {
  return bar().a != 1;
}
