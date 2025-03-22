// expected value: 0

int main() {
  int a = (int){0};
  int *b = &(((int){10}));
  int c = {{{{{{{{{{{{20}}}}}}}}}}}};
  
  if (a) {
    return 1;
  }

  if (*b != 10) {
    return 2;
  }

  if (c != 20) {
    return 3;
  }
}
