// expected value: 50
int main() {
  int a = 7, pb = 301, pc = 2450;

  int b = 0 - pb;
  int c = 0 - pc;

  int b_squared = pb * pb;
  int four_ac = 4 * a * c;

  int discrim = b_squared - four_ac;

  // if (discrim < 0) { return 255; }

  int guess = 1;
  while ((guess * guess) - discrim) {
    guess = guess + 1;
  }

  int tophalf = (0 - b) + (guess);
  return tophalf / (2 * a);
}
