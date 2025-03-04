// expected value: 194

int main() {
  const char foo[] = "bbba";

  char bar[] = "bbba";
  bar[0] = '_';

  return foo[3] + bar[3];
}
