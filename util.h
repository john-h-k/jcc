#ifndef UTIL_H
#define UTIL_H

// TODO: seperate bool/noreturn and other version-dependent stuff into its own
// header

#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#if __STDC_VERSION__ >= 202311L
#define STDC_C23 1
#elif __STDC_VERSION__ >= 201710L
#define STDC_C18 1
#elif __STDC_VERSION__ >= 201112L
#define STDC_C11 1
#else
#error "jcc only supports C11 or later"
#endif


#if STDC_C23 && __GNUC__
#define PRINTF_ARGS(idx) [gcc::format(printf, idx + 1, idx + 2)]
#elif __GNUC__
#define PRINTF_ARGS(idx) __attribute__ ((format (printf, idx + 1, idx + 2)))
#else
#define PRINTF_ARGS(idx)
#endif


#ifdef __has_feature
#define HAS_FEATURE(name) __has_feature(name)
#else
#define HAS_FEATURE(name) 0
#endif

#if HAS_FEATURE(memory_sanitizer) || defined(MEMORY_SANITIZER) ||              \
    defined(__SANITIZE_MEMORY__)
#define MSAN 1
#else
#define MSAN 0
#endif

#if HAS_FEATURE(address_sanitizer) || defined(ADDRESS_SANITIZER) ||            \
    defined(__SANITIZE_ADDRESS__)
#define ASAN 1
#else
#define ASAN 0
#endif

#if HAS_FEATURE(hwaddress_sanitizer) || defined(HWADDRESS_SANITIZER) ||        \
    defined(__SANITIZE_HWADDRESS__)
#define HWASAN 1
#else
#define HWASAN 0
#endif

#if HAS_FEATURE(thread_sanitizer) || defined(THREAD_SANITIZER) ||              \
    defined(__SANITIZE_THREAD__)
#define TSAN 1
#else
#define TSAN 0
#endif

#if HAS_FEATURE(undefined_behavior_sanitizer) ||                               \
    defined(UNDEFINED_BEHAVIOR_SANITIZER)
#define UBSAN 1
#else
#define UBSAN 0
#endif

#if ASAN || MSAN || TSAN || UBSAN
#define SANITIZER_PRINT_STACK_TRACE
#include "sanitizer/common_interface_defs.h" // __sanitizer_print_stack_trace
#endif

#if STDC_C23
#define NORETURN [[noreturn]]
#else
#include <stdnoreturn.h>
#define NORETURN noreturn
#endif

#define ROUND_UP(value, pow2) (((value) + ((pow2) - 1ull)) & ~((pow2) - 1ull))

#if STDC_C23 && __GNUC__
#define UNUSED_ARG(arg) [gcc::unused] arg
#elif __GNUC__
#define UNUSED_ARG(arg) __attribute__((__unused__)) arg
#else
#define UNUSED_ARG(arg)
#endif

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

#define ARR_LENGTH(a) (sizeof((a)) / sizeof((a)[0]))

static inline size_t num_digits(size_t num) {
  return (num ? (size_t)log10(num) : 0) + 1;
}

static inline void debug_print_stack_trace(void) {
#ifdef SANITIZER_PRINT_STACK_TRACE
  __sanitizer_print_stack_trace();
#endif
}

static inline int popcntl(unsigned long long l) {
#if defined(__has_builtin) && __has_builtin(__builtin_popcountll)
  return __builtin_popcountll(l);
#else
  todo("lzcnt not implemented outside of `__builtin_popcountll`");
#endif
}

static inline int tzcnt(unsigned long long l) {
#if defined(__has_builtin) && __has_builtin(__builtin_clzll)
  return __builtin_ctzll(l);
#else
  todo("lzcnt not implemented outside of `__builtin_clzll`");
#endif
}

static inline int lzcnt(unsigned long long l) {
#if defined(__has_builtin) && __has_builtin(__builtin_clzll)
  return __builtin_clzll(l);
#else
  todo("lzcnt not implemented outside of `__builtin_clzll`");
#endif
}

#define FMTPRINT(file, message, format)                                        \
  do {                                                                         \
    va_list v;                                                                 \
    va_start(v, format);                                                       \
    fprintf(file, message);                                                    \
    vfprintf(file, format, v);                                                 \
    fprintf(file, "\n");                                                       \
    va_end(v);                                                                 \
  } while (0);

#define EXIT_FAIL(code)                                                        \
  debug_print_stack_trace();                                                   \
  raise(SIGINT);                                                               \
  exit(code);

#if __clang__
#define START_NO_UNUSED_ARGS \
  _Pragma("clang diagnostic push") \
  _Pragma("clang diagnostic ignored \"-Wunused-parameter\"")
#elif __GNUC__
#define START_NO_UNUSED_ARGS \
  _Pragma("GCC diagnostic push") \
  _Pragma("GCC diagnostic ignored \"-Wunused-parameter\"")
#endif

#if __clang__
#define END_NO_UNUSED_ARGS \
  _Pragma("clang diagnostic pop")
#elif __GNUC__
#define END_NO_UNUSED_ARGS \
  _Pragma("GCC diagnostic pop")
#endif

#define TODO_FUNC(sig) \
  START_NO_UNUSED_ARGS \
  sig { todo(__func__); } \
  END_NO_UNUSED_ARGS


PRINTF_ARGS(0) NORETURN static inline void todo(const char *msg, ...) {
  FMTPRINT(stderr, "`todo` hit, program exiting: ", msg);
  EXIT_FAIL(-2);
}

PRINTF_ARGS(0) NORETURN static inline void unreachable(const char *msg, ...) {
  FMTPRINT(stderr, "`unreachable` hit, program exiting: ", msg);
  EXIT_FAIL(-2);
}

PRINTF_ARGS(0) NORETURN static inline void bug(const char *msg, ...) {
  FMTPRINT(stderr, "`bug` hit, program exiting: ", msg);
  EXIT_FAIL(-2);
}

// present in all mode, always causes program exit if fails
PRINTF_ARGS(1) static inline void invariant_assert(bool b, const char *msg, ...) {
  if (!b) {
    FMTPRINT(stderr, "invariant_assertion failed, program exiting: ", msg);
    EXIT_FAIL(-1);
  }
}

static inline void breakpoint(void) { raise(SIGINT); }

#if NDEBUG
PRINTF_ARGS(1) static inline void debug_assert(bool, const char *, ...) {}
#else
PRINTF_ARGS(1) static inline void debug_assert(bool b, const char *msg, ...) {
  if (!b) {
    FMTPRINT(stderr, "debug_assertion failed, program exiting: ", msg);
    EXIT_FAIL(-1);
  }
}
#endif

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

static inline char *malloc_strcpy(const char *s) {
  size_t len = strlen(s);

  char *cp = nonnull_malloc(len * sizeof(*s));

  // use memcpy as we already have len
  memcpy(cp, s, len * sizeof(*s));

  return cp;
}

#endif
