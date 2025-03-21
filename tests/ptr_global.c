// expected value: 34

int a[2] = { 5, 7 };

int *p0 = a;
int *p1 = a + 1;
int *p2 = &a[0];
int *p3 = &a[1];

struct foo {
  int a;
};

struct foo f = { 10 };
int *p4 = &f.a;

int main() {
  return *p0 + *p1 + *p2 + *p3 + *p4;
}
