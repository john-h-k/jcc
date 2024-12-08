// expected value: 4

#define FOO \
 4

#define BAR

#define buzz 10
#undef buzz

#ifdef FIZZ
#define buzz 0
invalid stuff
#else
#define BAT
#endif

int main() {
  int buzz = 1;
  return FOO BAR BAT;
}
