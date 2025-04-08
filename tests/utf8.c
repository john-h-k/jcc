// expected value: 1

double foo(double xₖ, double xₖ₊₁) {
  return xₖ₊₁ - xₖ;
}

int main() {
  if (foo(1, 2) != 1) {
    return 2;
  }

  int 😁 = 1;
  return 😁;
}
