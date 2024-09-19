// expected value: 24

int main() {
  char a;
  short b;
  unsigned c;
  unsigned long long d;
  char *p;
  return sizeof a + sizeof(b) + sizeof c + sizeof(d) + sizeof p + sizeof *p;
}
