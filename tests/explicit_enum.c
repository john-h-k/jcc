// expected value: 45

enum foo { FOUR = 4, FIVE, TWO = 2, THREE };

int main() { return (FOUR + FIVE) * (TWO + THREE); }
