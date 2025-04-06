// expected value: 0
// flags-darwin: -Wl,-U,_foo
// flags-linux: -Wl,--unresolved-symbols=ignore-all

struct c;

__attribute__((weak))
extern struct c foo;

int main() {
  return (int)(long)&foo;
}
