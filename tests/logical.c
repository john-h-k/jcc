// expected value: 5

int main() {   
  int a = 1;

  int b = !a;

  int c = 0;
  int d = 0;

  if (!a && ++b) {
    c = 5;
  }

  if (a || ++b) {
    d = 7;
  }

  int s = 0;

  int e = 8, f = 10;
  while (e && f) {
    e--;
    s++;
  }

  int x = 8, y = 10;
  while (x || y) {
    if (x) {
      x--;
    }
    y--;
    s--;
  }

  return b + c + d + s;
}
