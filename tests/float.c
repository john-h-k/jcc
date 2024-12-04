// expected value: 7

int main() {
  float a = 7.4f;

  int *p = (int *)&a;

  return *p;
}
