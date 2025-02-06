// expected value: 0

#define MIN(a, b) ((a) < (b) ? (a) : (b))

int main() {
  return MIN(0, MIN(5, 8));
}
