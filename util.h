#ifndef UTIL_H
#define UTIL_H

// TODO: seperate bool/noreturn and other version-dependent stuff into its own
// header

#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
#error "do not compile jcc as C++"
#endif

#if __STDC_VERSION__ >= 202311L
#define STDC_C23 1
#elif __STDC_VERSION__ >= 201710L
#define STDC_C18 1
#elif __STDC_VERSION__ == 201112L
#define STDC_C11 1
#else
#error "jcc only supports C11 or later"
#endif

#ifdef UTIL_MACRO_DEBUG

#ifdef __clang__
#pragma message "Compiler is clang"
#elif __GNUC__
#warn "Compiler is GCC"
#else
#pragma message "unrecognised compiler"
#endif

#if STDC_C23
#pragma message "C version is C23"
#elif STDC_C18
#pragma message "C version is C28"
#elif STDC_C11
#pragma message "C version is C11"
#else
#define EXPAND_INNER(x) "unrecognised C version '" #x "'"
#define EXPAND(x) EXPAND_INNER(x)
#pragma message(EXPAND(__STDC_VERSION__))
#undef EXPAND
#undef EXPAND_INNER
#endif

#endif

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

#ifdef __has_feature
#define HAS_FEATURE(name) __has_feature(name)
#else
#define HAS_FEATURE(name) 0
#endif

#ifdef __has_builtin
#define HAS_BUILTIN(name) __has_builtin(name)
#else
#define HAS_BUILTIN(name) 0
#endif

#ifdef __has_attribute
#define HAS_ATTRIBUTE(name) __has_attribute(name)
#else
#define HAS_ATTRIBUTE(name) 0
#endif

#ifdef __has_c_attribute
#define HAS_C_ATTRIBUTE(name) __has_c_attribute(name)
#else
#define HAS_C_ATTRIBUTE(name) 0
#endif

#if STDC_C23 && HAS_C_ATTRIBUTE(flag_enum)
#define FLAG_ENUM [gnu::flag_enum]
#elif HAS_ATTRIBUTE(flag_enum)
#define FLAG_ENUM __attribute__((flag_enum))
#else
#define FLAG_ENUM
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

#if __GNUC__ && STDC_32
#define TRYFORCEINLINE [[gnu::always_inline]] inline
#elif __GNUC__
#define TRYFORCEINLINE __attribute__((always_inline)) inline
#else
#define TRYFORCEINLINE inline
#endif

#define ROUND_UP(value, pow2) (((value) + ((pow2) - 1ull)) & ~((pow2) - 1ull))

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

#define ARR_LENGTH(a) (sizeof((a)) / sizeof((a)[0]))

static inline size_t num_digits(size_t num) {
  return (num ? (size_t)log10(num) : 0) + 1;
}

static inline void debug_print_stack_trace(void) {
#ifdef SANITIZER_PRINT_STACK_TRACE
  __sanitizer_print_stack_trace();
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

#define MACRO_FMTPRINT(file, message, ...)                                        \
  do {                                                                         \
    fprintf(file, message);                                                    \
    fprintf(file, __VA_ARGS__);                                                 \
    fprintf(file, "\n");                                                       \
  } while (0);

#define EXIT_FAIL(code)                                                        \
  debug_print_stack_trace();                                                   \
  raise(SIGINT);                                                               \
  exit(code);

#if __clang__
#define START_NO_UNUSED_ARGS                                                   \
  _Pragma("clang diagnostic push")                                             \
      _Pragma("clang diagnostic ignored \"-Wunused-parameter\"")
#elif __GNUC__ && 0
#define START_NO_UNUSED_ARGS                                                   \
  _Pragma("GCC diagnostic push")                                               \
      _Pragma("GCC diagnostic ignored \"-Wunused-parameter\"")
#else
#define START_NO_UNUSED_ARGS
#endif

#if __clang__
#define END_NO_UNUSED_ARGS _Pragma("clang diagnostic pop")
#elif __GNUC__ && 0
#define END_NO_UNUSED_ARGS _Pragma("GCC diagnostic pop")
#else
#define END_NO_UNUSED_ARGS
#endif

#ifdef SIZE_T_MAX
#undef SIZE_T_MAX
#define SIZE_T_MAX (static_assert(false, "use SIZE_MAX instead"))
#endif

#define TODO_FUNC(sig)                                                         \
  START_NO_UNUSED_ARGS                                                         \
  sig { TODO(__func__); }                                                      \
  END_NO_UNUSED_ARGS

#define TODO(...) \
  do { \
    MACRO_FMTPRINT(stderr, "`todo` hit, program exiting: ", __VA_ARGS__); \
    EXIT_FAIL(-2); \
  } while (0);

PRINTF_ARGS(0) NORETURN static inline void BUG(const char *msg, ...) {
  FMTPRINT(stderr, "`bug` hit, program exiting: ", msg);
  EXIT_FAIL(-2);
}

NORETURN static inline void unreachable(void) {
  fprintf(stderr, "`unreachable` hit, program exiting");
  EXIT_FAIL(-2);
}

PRINTF_ARGS(0) NORETURN static inline void unsupported(const char *msg, ...) {
  FMTPRINT(stderr, "", msg);
  EXIT_FAIL(-2);
}

// present in all mode, always causes program exit if fails
PRINTF_ARGS(1)
static inline void invariant_assert(bool b, const char *msg, ...) {
  if (!b) {
    FMTPRINT(stderr, "invariant_assertion failed, program exiting: ", msg);
    EXIT_FAIL(-1);
  }
}

#if HAS_BUILTIN(__builtin_debugtrap)
TRYFORCEINLINE static void breakpoint(void) { __builtin_debugtrap(); }
#else
TRYFORCEINLINE static void breakpoint(void) { raise(SIGINT); }
#endif

#if NDEBUG
PRINTF_ARGS(1) static inline void DEBUG_ASSERT(bool, const char *, ...) {}
#else
PRINTF_ARGS(1) static inline void DEBUG_ASSERT(bool b, const char *msg, ...) {
  if (!b) {
    FMTPRINT(stderr, "debug_assertion failed, program exiting: ", msg);
    EXIT_FAIL(-1);
  }
}
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
#if HAS_BUILTIN(__builtin_ctzll)
    return __builtin_ctzll(l);
#else
    if (l == 0) return sizeof(l) * 8;

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
    return __builtin_clzll(l);
#else
    if (l == 0) return sizeof(l * 8);

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
  return (sizeof(num) * 8) - lzcnt(num) - 1;
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

static inline void fprint_str(FILE *file, const char *input) {
  DEBUG_ASSERT(file, "null arg");

  if (!input) {
    fprintf(file, "(null)");
    return;
  }

  fputc('"', file);

  while (*input) {
    switch (*input) {
    case '\\':
      fputs("\\\\", file);
      break;
    case '\"':
      fputs("\\\"", file);
      break;
    case '\n':
      fputs("\\n", file);
      break;
    case '\t':
      fputs("\\t", file);
      break;
    case '\r':
      fputs("\\r", file);
      break;
    case '\b':
      fputs("\\b", file);
      break;
    case '\f':
      fputs("\\f", file);
      break;
    case '\v':
      fputs("\\v", file);
      break;
    default:
      if (isprint((unsigned char)*input)) {
        fputc(*input, file);
      } else {
        fprintf(file, "\\x%02x", (unsigned char)*input);
      }
      break;
    }
    input++;
  }

  fputc('"', file);
}

#endif
