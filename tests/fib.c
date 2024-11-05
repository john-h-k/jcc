// Expected value: 89

int fib(int n) {
  if (n == 0) {
    return n;
  }

  if (n == 1) {
    return n;
  }

  return fib(n - 1) + fib(n - 2);
}

int main() { return fib(11); }
