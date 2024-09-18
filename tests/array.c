// expected value: 12

int main() {
  int a[7];
  for (int i = 0; i < 7; i += 1) {
    a[i] = i * 2;
  }
  return a[6];
}
