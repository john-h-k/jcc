// expected value: 15

int main() {
  char a;
  short b;
  unsigned c;
  unsigned long long d;
  return sizeof a + sizeof(b) + sizeof c + sizeof(d);
}
