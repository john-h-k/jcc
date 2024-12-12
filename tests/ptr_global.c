// expected value: 9

int a[2];

int *p0 = a;
int *p1 = a + 1;


int main() {
  *p0 = 10;
  *p1 = -1;
  return *p0 + *p1;
}
