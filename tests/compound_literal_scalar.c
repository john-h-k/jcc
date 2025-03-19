// skip: not yet implemented
// expected value: 0

int main() {
  int a = (int){0};
  int *b = &(((int){10}));
  
  if (a) {
    return 1;
  }

  if (*b != 10) {
    return 2;
  }
}
