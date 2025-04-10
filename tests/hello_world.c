// Expected value: 0
// stdout: Hello, World!

// #include <stdio.h>
// int printf(const char *, ...);

#include <stdlib.h>

__DARWIN_OS_INLINE

foo

#if !defined(__DARWIN_OS_INLINE)
# if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
#        define __DARWIN_OS_INLINE static inline
# elif defined(__MWERKS__) || defined(__cplusplus)
#        define __DARWIN_OS_INLINE static inline
# else
#        define __DARWIN_OS_INLINE static __inline__
# endif
#endif


__DARWIN_OS_INLINE

// uint16_t
// _OSSwapnt16(
//   int a
// 	)
// {
	/* Reduces to 'rev16' with clang */
	// return (uint16_t)(_data << 8 | _data >> 8);
// }

// int main() {
//   int a = 1;
//   int b = a + 2;

//   printf("Hello, World!\n");
//   int c = b;
//   int k = c;
//   return 0;
// }
