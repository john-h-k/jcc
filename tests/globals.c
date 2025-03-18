#define NULL ((void *)0)

int a = 1;
int b;
static int c = 2;
static int d;
const int e = 3;

int *p1 = &a;
int *p2 = (void *)0;

int arr1[3] = {1, 2, 3};
int arr2[5] = {4, 5};
static int arr3[3];

struct S {
  int x;
  int y;
} s1 = {10, 20};

struct S s2;
static struct S s3;

union U {
  int i;
  float f;
} u1 = {42};

char str1[] = "hello";
char str2[10] = "hi";
static char str3[10];

volatile int v = 99;

extern int extern_var;

const int f = 10 + 5;
const int g = sizeof(int);
const double h = 3.14 * 2.0;

void *null_ptr = NULL;

int *p3 = (int *)0x1000;
int *p4 = &a + 1;

enum { CONST_ENUM = 42 };
int enum_test = CONST_ENUM;

bool bool_test = 100;

int main() {
  if (a != 1)
    return 1;
  if (b != 0)
    return 2;
  if (c != 2)
    return 3;
  if (d != 0)
    return 4;
  if (e != 3)
    return 5;

  if (p1 != &a || *p1 != 1)
    return 6;
  if (p2 != NULL)
    return 7;

  if (arr1[0] != 1 || arr1[1] != 2 || arr1[2] != 3)
    return 8;

  if (arr2[0] != 4 || arr2[1] != 5 || arr2[2] != 0 || arr2[3] != 0 ||
      arr2[4] != 0)
    return 9;

  if (arr3[0] != 0 || arr3[1] != 0 || arr3[2] != 0)
    return 10;

  if (s1.x != 10 || s1.y != 20)
    return 11;
  if (s2.x != 0 || s2.y != 0)
    return 12;
  if (s3.x != 0 || s3.y != 0)
    return 13;

  if (u1.i != 42)
    return 14;

  if (str1[0] != 'h' || str1[1] != 'e')
    return 15;

  if (str2[0] != 'h' || str2[1] != 'i')
    return 16;

  if (str3[0] != 0)
    return 17;

  if (v != 99)
    return 18;

  if (f != 15)
    return 19;

  if (g != sizeof(int))
    return 20;

  if (h != 6.28)
    return 21;

  if (null_ptr != NULL)
    return 22;

  if (enum_test != 42)
    return 23;

  if (p4 != &a + 1)
    return 24;

  if ((int)bool_test != 1) {
    return 25;
  }

  return 0;
}
