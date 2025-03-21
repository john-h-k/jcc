// expected value: 194

struct ch {
  const char *a;
};

struct arr {
  char buf[5];
};

struct ch glb_ch = { "hello" };
struct arr glb_arr = { "world" };

int printf(const char *,...);
int main() {
  if (glb_ch.a[0] != 'h') {
    return 1;
  }

  glb_ch.a = "bye";

  if (glb_ch.a[0] != 'b') {
    return 2;
  }

  if (glb_arr.buf[0] != 'w') {
    return 3;
  }

  glb_arr.buf[0] = 'x';

  if (glb_arr.buf[0] != 'x') {
    return 4;
  }

  const char foo[] = "bbba";

  char bar[] = "bbba";
  bar[0] = '_';

  return foo[3] + bar[3];
}
