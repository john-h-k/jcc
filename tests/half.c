// arch: arm64
// expected value: 3

int main() {
  _Float16 a = 0;
  _Float16 b = 2;

  __fp16 c = 1;
  __fp16 d = 3;

  _Float16 *p = &a;
  *p = 1;

  return ((a + b) + (c * d)) / 2;
}
