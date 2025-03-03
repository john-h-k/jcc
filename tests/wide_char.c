// expected value: 97

int main() {
  int c = L'a';

  const int *w = L"_a";

  return (c + w[1]) / 2;
}
