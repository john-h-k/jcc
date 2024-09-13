// expected value: 0
// stdout: "

int puts(const char *s);

int main() {
  const char *foo = "\"";

  puts(foo);
}
