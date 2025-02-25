// Expected value: 6

enum foo { ZERO, ONE, TWO, THREE };

enum bar { B_TWO = TWO, B_ONE = ONE };

int main() { return THREE + B_TWO + B_ONE ; }
