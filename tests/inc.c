// expected value: 1

int foo(int a) {
  return a;
}

int main() {
  int a = 0;
  int b = a++;

  int c = 0;
  int d = ++c;

  int e = 0;
  int f = foo(e++);

  if (e != 1) {
    return 2;
  }

  if (f != 0) {
    return 3;
  }

  return a == 1 && b == 0 && c == 1 && d == 1;
}
