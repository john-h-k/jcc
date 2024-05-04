
int main() {
  unsigned int op1, op2, op2_shifted, sum;

  op1 = 5;
  op2 = 7;

  sum = 0;
  op2_shifted = op2;

  // while (op1 != 0) {
  while (op1) {
    if (op1 & 1) {
      sum = sum + op2_shifted;
    }
    op2_shifted = op2_shifted << 1;
    op1 = op1 >> 1;
  }

  return sum;
}
