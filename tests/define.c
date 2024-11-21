// expected value: 4

#define FOO \
 4

#define BAR

#define buzz 10
#undef buzz

int main() {
  int buzz = 1;
  return FOO BAR;
}
