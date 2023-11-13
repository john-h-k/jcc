int main() {
  int a = 1;
  int b = 7;
  {
    int a = 2;
    b = 3;
  }

  return a + b;
}
