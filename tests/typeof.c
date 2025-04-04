// expected value: 0

#define CHECK_TY(ty, expr, n)                                                  \
  do {                                                                         \
    if (_Generic((expr), ty: 0, default: 1)) {                                 \
      return n;                                                                \
    }                                                                          \
  } while (0)

int main() {
  int a = 0;
  typeof(a) b = a;
  const typeof (b) *c = &b;

  typeof(c) d = c;

  CHECK_TY(int, b, 1);
  CHECK_TY(const int *, c, 2);
  CHECK_TY(const int *, d, 3);

  const int e = 0;
  typeof(e) *f = &e;
  typeof_unqual(e) *g = (void *)&e;
  const typeof_unqual(e) *h = &e;

  CHECK_TY(const int *, &e, 4);
  CHECK_TY(const int *, f, 5);
  CHECK_TY(int *, g, 6);
  CHECK_TY(const int *, h, 7);
}
