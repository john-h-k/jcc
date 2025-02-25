// expected value: 15

int main() {
  return sizeof(char) + sizeof(short) + sizeof(unsigned int) +
         sizeof(unsigned long long);
}
