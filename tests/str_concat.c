// expected value: 0
// stdout: foobarbaz

int puts(const char *);
int main() {
  const char *a = "foo" "bar" "baz";

  puts(a);

  return 0;
}
