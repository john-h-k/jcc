// expected value: 100

int foo(int a) {
  return a;
}

int bar(int (*f)(int), int idx, int arr[1]) {
  return f(arr[idx]);
}

int main() {
  int arr[] = { 100 };
  return bar(foo, 0, arr);
}
