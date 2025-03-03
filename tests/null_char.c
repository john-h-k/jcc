
const char *foo = "foo\0bar\0";

int main() {
  if (foo[0] != 'f') {
    return 1;
  }

  if (foo[1] != 'o') {
    return 2;
  }

  if (foo[2] != 'o') {
    return 3;
  }

  if (foo[3] != 0) {
    return 4;
  }

  if (foo[4] != 'b') {
    return 5;
  }

  if (foo[5] != 'a') {
    return 6;
  }

  if (foo[6] != 'r') {
    return 7;
  }

  if (foo[7] != 0) {
    return 8;
  }

  if (foo[8] != 0) {
    return 9;
  }

  return 0;
}
