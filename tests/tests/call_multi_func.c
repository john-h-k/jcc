// expected value: 20

int idouble(int a) { return a * 2; }

int add(int a, int b) { return a + b; }

int main() { return add(idouble(7), 6); }
