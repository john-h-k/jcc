// skip: needs second file for extern val (link fails)
// expected value: 0

struct c;

extern struct c foo;

int main() {
  struct c* p = &foo;  
}
