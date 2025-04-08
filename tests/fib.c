// Expected value: 89

// int fib(int nₖ) {
//   if (nₖ == 0) {
//     return nₖ;
//   }

//   if (nₖ == 1) {
//     return nₖ;
//   }

//   int nₖ₊₁= nₖ - 1;
//   int nₖ₊₂ = nₖ - 2;
//   return fib(nₖ₊₁) + fib(nₖ₊₂);
// }


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
