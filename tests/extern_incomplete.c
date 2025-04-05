// expected value: 0
// flags: -Wl,-U,_foo

struct c;

__attribute__((weak))
extern struct c foo;

int main() {
  return (int)(long)&foo;
}
