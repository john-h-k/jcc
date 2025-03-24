// expected value: 0
// stdout: string"foo bar\n"bar("foo bar\n")

#define MIN(a, b) ((a) < (b) ? (a) : (b))

#define STRINGIFY(x) # x

#define CONCAT0(a, b) a ## b
#define CONCAT1(a, b) foo_ ## a ## b
#define CONCAT2(a, b) a ## b ## _bar
#define CONCAT3(a, b) foo_ ## a ## b ## _bar

#define GET(a, b, c) a b c

#define AB a ## b

#define EXP1(l, r) l ## r

int puts(const char *);

#define foo bar

int main() {
  int EXP1(a_, foo) = 0;
  if (a_foo) {
    return 1;
  }

  int ab = 0;
  int foo_ab = 1;
  int ab_bar = 2;
  int foo_a0_bar = 3;

  int k = GET(,,4);
  if (k != 4) {
    return 1;
  }

  puts(STRINGIFY(string) STRINGIFY("foo bar\n") STRINGIFY(bar("foo bar\n")));

  int val = 0;

  // FIXME: at some point support concatenating operators like this
  // val CONCAT0(+, +);

  // if (val != 1) {
  //   return 1;
  // }

  CONCAT0(v, al += 1);

  if (val != 1) {
    return 2;
  }

  if (CONCAT0(1, 2) != 12) {
    return 3;
  }

  if (AB != ab) {
    return 4;
  }

  if (CONCAT0(a, b) != ab) {
    return 5;
  }

  if (CONCAT1(a, b) != foo_ab) {
    return 6;
  }

  if (CONCAT2(a, b) != ab_bar) {
    return 7;
  }

  if (CONCAT3(a, 0) != foo_a0_bar) {
    return 8;
  }

  return MIN(0, MIN(5, 8));
}
