// expected value: 0

enum ty {
  TY_INT,
  TY_FLOAT,
  TY_DOUBLE,
  TY_CHAR,
  TY_CHARP,
  TY_INTP,
  TY_UNKNOWN,
};

#define TYPE_NAME(x)                                                           \
  _Generic((x),                                                                \
      int: TY_INT,                                                             \
      float: TY_FLOAT,                                                         \
      double: TY_DOUBLE,                                                       \
      char: TY_CHAR,                                                           \
      char *: TY_CHARP,                                                        \
      int *: TY_INTP,                                                          \
      default: TY_UNKNOWN)

int main() {
  int a = 5;
  float b = 5.5f;
  double c = 10.10;
  char d = 'x';
  char *e = "hello";
  int *f = &a;

  if (_Generic((a),
      int: TY_INT,
      float: TY_FLOAT,
      double: TY_DOUBLE,
      char: TY_CHAR,
      char *: TY_CHARP,
      int *: TY_INTP,
      default: TY_UNKNOWN) != TY_INT) {
    return 1;
  }

  if (TYPE_NAME(a) != TY_INT) {
    return 2;
  }

  if (TYPE_NAME(b) != TY_FLOAT) {
    return 3;
  }

  if (TYPE_NAME(c) != TY_DOUBLE) {
    return 4;
  }

  if (TYPE_NAME(d) != TY_CHAR) {
    return 5;
  }

  if (TYPE_NAME(e) != TY_CHARP) {
    return 6;
  }

  if (TYPE_NAME(f) != TY_INTP) {
    return 7;
  }
}
