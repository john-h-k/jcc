// expected value: 25

int main() {
  int i = 0;
  while (1) {
    if (i == 10) {
      break;
    }

    i++;
  }

  int j = 0;
  while (1) {
    if (j != 15) {
      j++;
      continue;
    }

    break;
  }

  return i + j;
}
