// expected value: 0

int main() {
  int md0[2][2] = { {0, 1}, {2, 3} };

  for (int i = 0; i < 2; i++) {
    for (int j = 0; j < 2; j++) {
      if (md0[i][j] != (i * 2) + j) {
        return (i * 2) + j;
      }
    }
  }

  return 0;
}
