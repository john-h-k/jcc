// expected value: 91

int main() {
  struct S {
    struct S *p;
    int x;
  } s;

  s.x = 91;
  s.p = &s;
  return s.p->p->p->p->p->x;
}
