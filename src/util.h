#ifndef UTIL_H
#define UTIL_H

// TODO: seperate bool/noreturn and other version-dependent stuff into its own
// header

#include <math.h>
#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef ptrdiff_t ssize_t;

#ifdef __cplusplus
#error "do not compile jcc as C++"
#endif

#include "compinfo.h"

#ifdef INT128_C
#define HAS_INT128 1
#elif __clang__ || __GNUC__
#define HAS_INT128 1
typedef __int128 int128_t;
typedef unsigned __int128 uint128_t;
#elif STDC_C23 && BITINT_MAXWIDTH >= 128
typedef _BitInt(128) int128_t;
typedef unsigned _BitInt(128) uint128_t;
#define HAS_INT128 1
#endif

#if STDC_C23 && __GNUC__
#define PRINTF_ARGS(idx) [gnu::format(printf, idx + 1, idx + 2)]
#elif __GNUC__
#define PRINTF_ARGS(idx) __attribute__((format(printf, idx + 1, idx + 2)))
#else
#define PRINTF_ARGS(idx)
#endif

#if STDC_C23 && HAS_C_ATTRIBUTE(flag_enum)
#define FLAG_ENUM [gnu::flag_enum]
#elif HAS_ATTRIBUTE(flag_enum)
#define FLAG_ENUM __attribute__((flag_enum))
#else
#define FLAG_ENUM
#endif

#if ASAN || MSAN || TSAN || UBSAN
#define SANITIZER_PRINT_STACK_TRACE
#include <sanitizer/common_interface_defs.h> // __sanitizer_print_stack_trace
#endif

#if STDC_C23
#define NORETURN [[noreturn]]
#else
#include <stdnoreturn.h>
#define NORETURN noreturn
#endif

#if __GNUC__ && STDC_23
#define TRYFORCEINLINE [[gnu::always_inline]] inline
#define NOINLINE [[gnu::noinline]]
#elif __GNUC__
#define TRYFORCEINLINE __attribute__((always_inline)) inline
#define NOINLINE __attribute__((noinline))
#else
#define TRYFORCEINLINE inline
#define NOINLINE
#endif

#define ROUND_UP(value, pow2) (((value) + ((pow2)-1ull)) & ~((pow2)-1ull))

#define ISPOW2(value) (popcntl((value)) == 1)
#define ILOG2(value) (sizeof(unsigned long long) * 8 - 1 - lzcnt((value)))

#if STDC_C23 && __GNUC__
#define UNUSED_ARG(arg) [gnu::unused] arg
#define UNUSED [gnu::unused]
#elif __GNUC__
#define UNUSED_ARG(arg) __attribute__((__unused__)) arg
#define UNUSED __attribute__((__unused__))
#else
#define UNUSED_ARG(arg)
#define UNUSED
#endif

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

// fails on one-length arrays, but that is okay because who uses one-length arrays
// ensures it is not a pointer type
#define ARR_LENGTH(a) ((void)sizeof(struct { int _your_array_is_a_pointer_or_one_length[(sizeof((a)) / sizeof((a)[0])) > 1 ? 1 : -1]; }), sizeof((a)) / sizeof((a)[0]))

static inline size_t num_digits(size_t num) {
  return (num ? (size_t)log10(num) : 0) + 1;
}

#ifdef SANITIZER_PRINT_STACK_TRACE
static inline void debug_print_stack_trace(void) {
  __sanitizer_print_stack_trace();
}
#elif !defined(NDEBUG) && __has_include(<execinfo.h>) && __has_include(<unistd.h>) && __has_include(<errno.h>)
#define UTIL_STACK_TRACE_IMPL
void debug_print_stack_trace(void);
#else
static inline void debug_print_stack_trace(void) {
  
}
#endif


#if __clang__

#define DO_PRAGMA(x) _Pragma (#x)
#define PUSH_NO_WARN(warn)  \
  _Pragma("clang diagnostic push")                                             \
      DO_PRAGMA(clang diagnostic ignored warn)
#define POP_NO_WARN() _Pragma("clang diagnostic pop")

#elif __GNUC__ && 0
// this does not work for some reason? errors about "expected declaration specifiers before '#pragma'"
// so disabled

#define DO_PRAGMA(x) _Pragma (#x)
#define PUSH_NO_WARN(warn) \
  DO_PRAGMA(GCC diagnostic push)                                              \
      DO_PRAGMA(GCC diagnostic ignored warn)

#define POP_NO_WARN() DO_PRAGMA(GCC diagnostic pop)

#else

#define PUSH_NO_WARN(warn)
#define POP_NO_WARN()

#endif

// TEMP:
#ifdef __JCC__
#undef va_start
#define va_start(...)
#undef va_end
#define va_end(...)
#endif

#define START_NO_UNUSED_ARGS PUSH_NO_WARN("-Wunused-parameter")
#define END_NO_UNUSED_ARGS POP_NO_WARN()

#ifdef SIZE_T_MAX
#undef SIZE_T_MAX
#define SIZE_T_MAX (static_assert(false, "use SIZE_MAX instead"))
#endif

#define FMTPRINT(file, message, format)                                        \
  do {                                                                         \
    va_list v;                                                                 \
    va_start(v, format);                                                       \
    fprintf(file, message);                                                    \
    vfprintf(file, format, v);                                                 \
    fprintf(file, "\n");                                                       \
    va_end(v);                                                                 \
  } while (0);

#define MACRO_FMTPRINT(file, message, ...)                                     \
  do {                                                                         \
    fprintf(file, "%s:%d in %s: ", __FILE__, __LINE__, __func__);              \
    fprintf(file, message);                                                    \
    fprintf(file, __VA_ARGS__);                                                \
    fprintf(file, "\n");                                                       \
  } while (0);

#define EXIT_FAIL(code)                                                        \
  debug_print_stack_trace();                                                   \
  raise(SIGINT);                                                               \
  exit(code);

#define TODO_FUNC(sig)                                                         \
  START_NO_UNUSED_ARGS                                                         \
  sig { TODO(__func__); }                                                      \
  END_NO_UNUSED_ARGS

#define TODO(...)                                                              \
  do {                                                                         \
    MACRO_FMTPRINT(stderr, "`TODO` hit, program exiting  ", __VA_ARGS__);        \
    EXIT_FAIL(-2);                                                             \
  } while (0);

#define BUG(...)                                                               \
  do {                                                                         \
    MACRO_FMTPRINT(stderr, "`BUG` hit, program exiting:  ", __VA_ARGS__);       \
    EXIT_FAIL(-2);                                                             \
  } while (0);

#if NDEBUG
#define DEBUG_ASSERT(b, ...) (void)(b)
#else
#define DEBUG_ASSERT(b, ...)                                                   \
  util_debug_assert(b, #b, __func__, __FILE__, __LINE__, __VA_ARGS__)

PRINTF_ARGS(5)
 void util_debug_assert(bool b, const char *cond, const char *func,
                              const char *file, int line, const char *msg,
                              ...);

 #endif

NORETURN void unreachable(void);

PRINTF_ARGS(0) NORETURN void unsupported(const char *msg, ...);

// present in all mode, always causes program exit if fails
PRINTF_ARGS(1) static inline void invariant_assert(bool b, const char *msg, ...) {
  if (!b) {
#ifdef __JCC__
    // doesn't support varargs
    fprintf(stderr, "invariant_assertion failed, program exiting");
#else
    FMTPRINT(stderr, "invariant_assertion failed, program exiting: ", msg);
#endif
    EXIT_FAIL(-1);
  }
}

#if HAS_BUILTIN(__builtin_debugtrap)
#define BREAKPOINT() __builtin_debugtrap()
#else
#define BREAKPOINT() raise(SIGINT)
#endif

static inline unsigned long long rotateright64(unsigned long long value,
                                               unsigned int amount) {
#if HAS_BUILTIN(__builtin_rotateright64)
  return __builtin_rotateright64(value, amount);
#else
  amount %= sizeof(value) * 8; // Ensure amount is within [0, 63]
  return (value >> amount) | (value << ((sizeof(value) * 8) - amount));
#endif
}

static inline int popcntl(unsigned long long l) {
#if HAS_BUILTIN(__builtin_popcountll)
  return __builtin_popcountll(l);
#else
  int count = 0;
  while (l) {
    count += l & 1;
    l >>= 1;
  }
  return count;
#endif
}

static inline int tzcnt(unsigned long long l) {
  DEBUG_ASSERT(l != 0, "zero is UB");

#if HAS_BUILTIN(__builtin_ctzll)
  return __builtin_ctzll(l);
#else
  if (l == 0) {
    return sizeof(l) * 8;
  }

  int count = 0;
  while ((l & 1) == 0) {
    count++;
    l >>= 1;
  }
  return count;
#endif
}

static inline int lzcnt(unsigned long long l) {
#if HAS_BUILTIN(__builtin_clzll)
  if (l == 0) {
    return sizeof(l) * 8;
  }
  return __builtin_clzll(l);
#else
  if (l == 0) {
    return sizeof(l) * 8;
  }

  int count = 0;
  for (int i = (sizeof(l) * 8) - 1; i >= 0; i--) {
    if ((l >> i) & 1) {
      break;
    }
    count++;
  }
  return count;
#endif
}

static inline unsigned long long ilog2(unsigned long long num) {
  return (sizeof(num) * 8) - (unsigned)lzcnt(num) - 1;
}

static inline void *nonnull_malloc(size_t size) {
  if (size == 0) {
    return NULL;
  }

  void *ptr = malloc(size);

  invariant_assert(ptr, "`malloc` returned NULL (out-of-memory likely)");

  return ptr;
}

static inline void *nonnull_realloc(void *p, size_t size) {
  void *ptr = realloc(p, size);

  invariant_assert(ptr, "`realloc` returned NULL (out-of-memory likely)");

  return ptr;
}

void fprint_str(FILE *file, const char *input, size_t len);
void fprint_wstr(FILE *file, const char *input, size_t len);
bool try_parse_integer(const char *str, size_t len, unsigned long long *value);

#endif
