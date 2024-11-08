// expected value: 0

enum E *e;
const enum E *e1;
enum E const *e2;
struct S *s;
const struct S *s1;
struct S const *s2;

// typedef int (*fptr1)();
// int f1 (int (), int);
// typedef int (*fptr2)(int x);
// int f2 (int (int x), int);
// typedef int (*fptr3)(int);
// int f3 (int (int), int);
// typedef int (*fptr4[4])(int);
// int f4 (int (*[4])(int), int);
// typedef int (*fptr5)(fptr1);
// int f5 (int (int()), fptr1);

// int foo(int (int x)) {
  
// }

int main() {
  
}
