// expected value: 1

int main() {
  int x = 1;

  switch (x) {
    case 2:
      return 8;
    case 1:
      return 1;
    default:
      return 0;
  }
}
