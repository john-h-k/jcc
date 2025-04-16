// expected value: 5

// ignore the slightly weird code
// - it helps generate clearer IR for explaining lowering
int main() {
  int a = 10, b = 7;

  int answer;

  if (a % b) {
    answer = 5;
  } else {
    answer = 88;
  }

  return answer;
}
