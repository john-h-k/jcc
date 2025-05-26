// expected value: 0
// arch: aarch64

// TODO: these intrinsics on other platforms

int main() {
  int a = 7;

  if (__builtin_popcount(a) != 3) {
    return 1;
  }

  if (__builtin_clz(a) != 29) {
    return 2;
  }

  if (__builtin_ctz(a) != 0) {
    return 3;
  }

  int b = 0xFE915022;
  int c = __builtin_bswap32(b);

  if (c != 0x225091FE) {
    return 4;
  }

  unsigned long long d = 0xFE915022DEADBEEF;
  unsigned long long e = __builtin_bswap64(d);

  if (e != 0xEFBEADDE225091FE) {
    return 5;
  }
}
