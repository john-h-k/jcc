// expected value: 1

int foo(int a[]) {
  return a[0];
}

int main() {
  int a[1] = {1};

  return foo(a);
}
