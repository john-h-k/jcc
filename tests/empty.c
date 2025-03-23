// expected value: 88

#undef signed
#undef unsigned
#undef char
#undef short
#undef int
#undef __int20
#undef __int20__
#undef long
#define signed +0
#define unsigned +0
#define char +0
#define short +1
#define __int20 +2
#define __int20__ +2
#define int +2
#define long +4

bool a = __INTPTR_TYPE__;

// #if (__INTPTR_TYPE__ == 8 || __INTPTR_TYPE__ == 10)
// #define _INTPTR_EQ_LONGLONG
// #elif (__INTPTR_TYPE__ == 4 || __INTPTR_TYPE__ == 6)
// #define _INTPTR_EQ_LONG
// /* Note - the tests for _INTPTR_EQ_INT and _INTPTR_EQ_SHORT are currently
//    redundant as the values are not used.  But one day they may be needed
//    and so the tests remain.  */
// #elif __INTPTR_TYPE__ == 2
// #define _INTPTR_EQ_INT
// #elif (__INTPTR_TYPE__ == 1 || __INTPTR_TYPE__ == 3)
// #define _INTPTR_EQ_SHORT
// #else
// #error "Unable to determine type definition of intptr_t"
// #endif
// #if (__INT32_TYPE__ == 4 || __INT32_TYPE__ == 6)
// #define _INT32_EQ_LONG
// #elif __INT32_TYPE__ == 2
// /* Nothing to define because int32_t is safe to print as an int. */
// #else
// #error "Unable to determine type definition of int32_t"
// #endif

// #if (__INT8_TYPE__ == 0)
// #define __INT8 "hh"
// #elif (__INT8_TYPE__ == 1 || __INT8_TYPE__ == 3)
// #define __INT8 "h"
// #elif (__INT8_TYPE__ == 2)
// #define __INT8
// #elif (__INT8_TYPE__ == 4 || __INT8_TYPE__ == 6)
// #define __INT8 "l"
// #elif (__INT8_TYPE__ == 8 || __INT8_TYPE__ == 10)
// #define __INT8 "ll"
// #endif
// #if (__INT16_TYPE__ == 1 || __INT16_TYPE__ == 3)
// #define __INT16 "h"
// #elif (__INT16_TYPE__ == 2)
// #define __INT16
// #elif (__INT16_TYPE__ == 4 || __INT16_TYPE__ == 6)
// #define __INT16 "l"
// #elif (__INT16_TYPE__ == 8 || __INT16_TYPE__ == 10)
// #define __INT16 "ll"
// #endif
// #if (__INT32_TYPE__ == 2)
// #define __INT32
// #elif (__INT32_TYPE__ == 4 || __INT32_TYPE__ == 6)
// #define __INT32 "l"
// #elif (__INT32_TYPE__ == 8 || __INT32_TYPE__ == 10)
// #define __INT32 "ll"
// #endif
// #if (__INT64_TYPE__ == 2)
// #define __INT64
// #elif (__INT64_TYPE__ == 4 || __INT64_TYPE__ == 6)
// #define __INT64 "l"
// #elif (__INT64_TYPE__ == 8 || __INT64_TYPE__ == 10)
// #define __INT64 "ll"
// #endif
// #if (__INT_FAST8_TYPE__ == 0)
// #define __FAST8 "hh"
// #elif (__INT_FAST8_TYPE__ == 1 || __INT_FAST8_TYPE__ == 3)
// #define __FAST8 "h"
// #elif (__INT_FAST8_TYPE__ == 2)
// #define __FAST8
// #elif (__INT_FAST8_TYPE__ == 4 || __INT_FAST8_TYPE__ == 6)
// #define __FAST8 "l"
// #elif (__INT_FAST8_TYPE__ == 8 || __INT_FAST8_TYPE__ == 10)
// #define __FAST8 "ll"
// #endif
// #if (__INT_FAST16_TYPE__ == 1 || __INT_FAST16_TYPE__ == 3)
// #define __FAST16 "h"
// #elif (__INT_FAST16_TYPE__ == 2)
// #define __FAST16
// #elif (__INT_FAST16_TYPE__ == 4 || __INT_FAST16_TYPE__ == 6)
// #define __FAST16 "l"
// #elif (__INT_FAST16_TYPE__ == 8 || __INT_FAST16_TYPE__ == 10)
// #define __FAST16 "ll"
// #endif
// #if (__INT_FAST32_TYPE__ == 2)
// #define __FAST32
// #elif (__INT_FAST32_TYPE__ == 4 || __INT_FAST32_TYPE__ == 6)
// #define __FAST32 "l"
// #elif (__INT_FAST32_TYPE__ == 8 || __INT_FAST32_TYPE__ == 10)
// #define __FAST32 "ll"
// #endif
// #if (__INT_FAST64_TYPE__ == 2)
// #define __FAST64
// #elif (__INT_FAST64_TYPE__ == 4 || __INT_FAST64_TYPE__ == 6)
// #define __FAST64 "l"
// #elif (__INT_FAST64_TYPE__ == 8 || __INT_FAST64_TYPE__ == 10)
// #define __FAST64 "ll"
// #endif

// #if (__INT_LEAST8_TYPE__ == 0)
// #define __LEAST8 "hh"
// #elif (__INT_LEAST8_TYPE__ == 1 || __INT_LEAST8_TYPE__ == 3)
// #define __LEAST8 "h"
// #elif (__INT_LEAST8_TYPE__ == 2)
// #define __LEAST8
// #elif (__INT_LEAST8_TYPE__ == 4 || __INT_LEAST8_TYPE__ == 6)
// #define __LEAST8 "l"
// #elif (__INT_LEAST8_TYPE__ == 8 || __INT_LEAST8_TYPE__ == 10)
// #define __LEAST8 "ll"
// #endif
// #if (__INT_LEAST16_TYPE__ == 1 || __INT_LEAST16_TYPE__ == 3)
// #define __LEAST16 "h"
// #elif (__INT_LEAST16_TYPE__ == 2)
// #define __LEAST16
// #elif (__INT_LEAST16_TYPE__ == 4 || __INT_LEAST16_TYPE__ == 6)
// #define __LEAST16 "l"
// #elif (__INT_LEAST16_TYPE__ == 8 || __INT_LEAST16_TYPE__ == 10)
// #define __LEAST16 "ll"
// #endif
// #if (__INT_LEAST32_TYPE__ == 2)
// #define __LEAST32
// #elif (__INT_LEAST32_TYPE__ == 4 || __INT_LEAST32_TYPE__ == 6)
// #define __LEAST32 "l"
// #elif (__INT_LEAST32_TYPE__ == 8 || __INT_LEAST32_TYPE__ == 10)
// #define __LEAST32 "ll"
// #endif
// #if (__INT_LEAST64_TYPE__ == 2)
// #define __LEAST64
// #elif (__INT_LEAST64_TYPE__ == 4 || __INT_LEAST64_TYPE__ == 6)
// #define __LEAST64 "l"
// #elif (__INT_LEAST64_TYPE__ == 8 || __INT_LEAST64_TYPE__ == 10)
// #define __LEAST64 "ll"
// #endif
#undef signed
#undef unsigned
#undef char
#undef short
#undef int
#undef long

int main() {
  
  return 88;
}
