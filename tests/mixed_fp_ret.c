// expected value: 3

struct S { float x; double y; };

struct S f(double d) {
    struct S s;
    s.x = d;
    s.y = d;
    return s;
}

int main() {
  struct S s = f(1.5);
  return s.x + s.y;
}
