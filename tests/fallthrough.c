// expected value: 5

int main() {
  int x = 0;

  switch (x) {
    case 0:
      x++;
    case 1:
      x++;
    case 2:
      x++;
    case 3:
      x++;
    case 4:
      x++;
      break;
    case 5:
      x = 0;
  }

  return x;
}
