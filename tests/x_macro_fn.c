// expected value: 0

#define FOO_LIST \
  FIZZ(a, 1) \
  FIZZ(b, 2) \
  FIZZ(c, 3) \

int main() {
  #define FIZZ(x, y) int x = y;

  FOO_LIST;

  #undef FIZZ

  #define FIZZ(x, y) if (x != y) { return y; }

  FOO_LIST;

  #undef FIZZ
}
