// expected value: 12

int a[2] = { 5, 7 };

int *p0 = a;
int *p1 = a + 1;

int main() {
  return *p0 + *p1;
}
