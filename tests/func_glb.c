// expected value: 0
// stdout: hi

int puts(const char *);

int foo(void) {
  return 10;
}


int (*bar)(void) = foo;

struct fptr {
  int (*bar)(void);
  int (*p)(const char *);
} str = {
  foo,
  puts
};

int main() {
  if (bar() != 10) {
    return 1;
  }

  if (str.bar() != 10) {
    return 1;
  }

  str.p("hi");
}
