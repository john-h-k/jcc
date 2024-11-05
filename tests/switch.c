// expected value: 5

int main() {
  int x = 2;

  int a;

  switch (x) {
    case 2:
      a = 0;
      break;
    case 1:
      a = 1;
      break;
    default:
      a = 2;
      break;
  }

  int b;

  switch (x) {
    case 2:
      b = 0;
      break;
    case 1:
      b = 1;
      break;
  }

  int c = 5;

  return a + b + c;
}
