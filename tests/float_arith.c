// expected value: 0
// stdout: 1.000000 7.400000 98.660004 8.400000 106.060005 -97.660004 -90.260002
// 8814.791992 -9635.136719 -0.914859 89.345139

int printf(const char *format, ...);

int main() {
  float a = 1;
  float b = 7.4;
  double c = 98.66f;

  printf("%f %f %f ", a, b, c);

  float d = a + b;
  float e = b + c;

  float f = d - e;
  float g = d - c;

  float h = f * g;
  float i = f * c;

  float j = h / i;
  float k = h / c;

  printf("%f %f %f %f %f %f %f %f\n", d, e, f, g, h, i, j, k);
}
