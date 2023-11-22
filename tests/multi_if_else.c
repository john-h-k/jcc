// expected value: 91
int main() {
  int a, b, c;

  if (1) {
    a = 1;
  } else if (1) {
    a = 2;
  } else {
    a = 3;
  }

  if (0) {
    b = 10;
  } else if (1) {
    b = 20;
  } else {
    b = 30;
  }

  if (0) {
    c = 50;
  } else if (0) {
    c = 60;
  } else {
    c = 70;
  }

  return a + b + c;
}

