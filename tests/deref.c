// expected value: 20

int main() {
  int a;
  int *p = &a;
  *p = 10;
  return *p + a;
}
