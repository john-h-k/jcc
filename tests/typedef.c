// expected value: 7

void baz(int *p) {
  *p += 1;
}

typedef void *bazl;
typedef void *bazl;

typedef int my_int;

typedef void (*foo)(int *);
// it should accept duplicate
typedef void (*foo)(int *);

struct cg_state {
  int a;
};
typedef void (*target_codegen)(struct cg_state *state);

typedef struct foo {
  int a;
} bar;

int main() {
  my_int a = 1;
  struct foo b = {2};
  bar c = {3};

  foo f = baz;
  f(&a);

  return a + b.a + c.a;
}
