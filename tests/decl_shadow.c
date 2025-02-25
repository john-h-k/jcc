// expected value: 4
int main() {
  int a = 1;
  unsigned b = 7;
  {
    int a = 2;
    b = 3;
  }

  return a + b;
}
