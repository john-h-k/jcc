// skip: preproc prescan fixes needed
// no-compile

#define T(a, b, c) a + b + c;
#define FOO 1, 2, 3

int main() {
  return T(FOO);
}
