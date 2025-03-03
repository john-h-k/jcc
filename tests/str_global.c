// expected value: 65

const char *s = "hello";

int main() {
  if (s[0] != 'h') {
    return 1;
  }

  s = "A";
  return s[0];
}
