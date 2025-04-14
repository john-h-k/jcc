// expected value: 0
// stdout: "dog says woof woof bark!"
// std: c2y

#include <stdio.h>

int main () {
	const char* s = "this is not going to appear because it's going to be reassigned";
	defer printf(" bark!\"");
	defer printf("%s", s);
	defer {
		defer printf(" woof");
		printf(" says");
	}
	printf("\"dog");
	s = " woof";
	return 0;
}
