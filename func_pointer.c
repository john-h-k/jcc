// expected value: 3

int val;
int inc() { return val++; }

int main() {
  int (*a)() = inc;
  int (*b)() = &inc;

  return a() + b() + val;
}
