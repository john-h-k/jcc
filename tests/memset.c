// expected value: 0

void *memset(void *str, int c, unsigned long n);

int main() {
  char b[2000];
  memset(b, 0, 2000);

  for (int i = 0; i < 2000; i++) {
    if (b[i]) {
      return 1;
    }
  }

  memset(b, 32, 2000);

  for (int i = 0; i < 2000; i++) {
    if (b[i] != 32) {
      return 1;
    }
  }
}

