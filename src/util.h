#ifndef UTIL_H
#define UTIL_H

// TODO: seperate bool/noreturn and other version-dependent stuff into its own
// header

#include <assert.h>
#include <ctype.h>
#include <math.h>
#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// I _want_ to do this but feel like it may have some unknown side effects
// #ifdef true
// #undef true
// #define true ((bool)1)
// #endif

// #ifdef false
// #undef false
// #define false ((bool)0)
// #endif

typedef ptrdiff_t ssize_t;

#include "compinfo.h"

#ifdef INT128_C
#define HAS_INT128 1
#elif (__clang__ || __GNUC__) && defined(__SIZEOF_INT128__)
#define HAS_INT128 1
typedef __int128 int128_t;
typedef unsigned __int128 uint128_t;
#elif STDC_MIN(STDC_23) && BITINT_MAXWIDTH >= 128
typedef _BitInt(128) int128_t;
typedef unsigned _BitInt(128) uint128_t;
#define HAS_INT128 1
#endif

#if STDC_MIN_23 && __GNUC__
#define PRINTF_ARGS(idx) [[gnu::format(printf, idx + 1, idx + 2)]]
#elif STDC_MIN_23 && __JCC__
#define PRINTF_ARGS(idx) [[jcc::format(printf, idx + 1, idx + 2)]]
#elif __GNUC__ || __JCC__
#define PRINTF_ARGS(idx) __attribute__((format(printf, idx + 1, idx + 2)))
#else
#define PRINTF_ARGS(idx)
#endif

#if STDC_MIN_23 && __GNUC__
#define COLD [gnu::cold]
#elif __GNUC__
#define COLD __attribute__((cold))
#else
#define COLD
#endif

#if __GNUC__ || __clang__
#define LIKELY(x) __builtin_expect((x), 1)
#define UNLIKELY(x) __builtin_expect((x), 0)
#else
#define LIKELY(x) x
#define UNLIKELY(x) x
#endif

// FIXME: shouldn't need this check but temp included so `ir.h` doesn't need to
// import this header
#ifndef FLAG_ENUM
#if STDC_MIN_23 && HAS_C_ATTRIBUTE(flag_enum)
#define FLAG_ENUM [gnu::flag_enum]
#elif HAS_ATTRIBUTE(flag_enum)
#define FLAG_ENUM __attribute__((flag_enum))
#else
#define FLAG_ENUM
#endif
#endif

#if __has_include(<sanitizer/common_interface_defs.h>)
#define SANITIZER_PRINT_STACK_TRACE
#include <sanitizer/common_interface_defs.h> // __sanitizer_print_stack_trace
#endif

#if HAS_FEATURE(attribute_analyzer_noreturn)
#define NORETURN __attribute__((__noreturn__, __analyzer_noreturn__))
#endif

#if STDC_MIN_23
#ifndef NORETURN
#define NORETURN [[noreturn]]
#endif

#define DEPRECATED [[deprecated]]
#define FALLTHROUGH [[fallthrough]]
#define MAYBE_UNUSED [[maybe_unused]]
#define NODISCARD [[no_discard]]
#define UNSEQUENCED [[unsequenced]]
#define REPRODUCIBLE [[reproducible]]
#else
#ifndef NORETURN
#include <stdnoreturn.h>
#define NORETURN noreturn
#endif

#if HAS_ATTRIBUTE(__fallthrough__)
#define FALLTHROUGH __attribute__((__fallthrough__))
#else
#define FALLTHROUGH
#endif

#if HAS_ATTRIBUTE(__deprecated__)
#define DEPRECATED __attribute__((__deprecated__))
#else
#define DEPRECATED
#endif

// gcc errors with "ignored attribute" even though it claims support
#if HAS_ATTRIBUTE(__maybe_unused__) && !__GNUC__
#define MAYBE_UNUSED __attribute__((__maybe_unused__))
#else
#define MAYBE_UNUSED
#endif

#if HAS_ATTRIBUTE(__warn_unused_result__)
#define NODISCARD __attribute__((__warn_unused_result__))
#else
#define NODISCARD
#endif

#define UNSEQUENCED
#define REPRODUCIBLE
#endif

#if __GNUC__ && STDC_MIN_23
#define TRYFORCEINLINE [[gnu::always_inline]] inline
#define NOINLINE [[gnu::noinline]]
#elif __GNUC__
#define TRYFORCEINLINE __attribute__((always_inline)) inline
#define NOINLINE __attribute__((noinline))
#else
#define TRYFORCEINLINE inline
#define NOINLINE
#endif

#define CONST_CAST(x)                                                          \
  PUSH_NO_WARN("-Wcast-qual")                                                  \
  (x) POP_NO_WARN()

#if __clang__
#define ERR_EXPR(msg)                                                          \
  PUSH_NO_WARN("-Wgnu-statement-expression-from-macro-expansion")(void)({      \
    static_assert(0, msg);                                                     \
    0;                                                                         \
  })POP_NO_WARN()
#define ERR_EXPR_COND(cond, msg)                                               \
  PUSH_NO_WARN("-Wgnu-statement-expression-from-macro-expansion")(void)({      \
    static_assert(cond, msg);                                                  \
    0;                                                                         \
  })POP_NO_WARN()
#else
#define ERR_EXPR(msg) ((int)sizeof(char[-1]))
#define ERR_EXPR_COND(cond, msg) ((int)sizeof(char[(cond) ? 1 : -1]))
#endif

/********** Banned functions **********/

#define system(...)                                                            \
  (ERR_EXPR("'system' is banned, use 'syscmd' type instead"), 0)
#define fscanf(...)                                                            \
  (ERR_EXPR("'fscanf' is banned, causes all sorts of cache problems"), 0)

/**************************************/

#ifndef NDEBUG

#if HAS_BUILTIN(__builtin_dump_struct)
#define dbg(x)                                                                 \
  PUSH_NO_WARN("-Wformat-pedantic")                                            \
  __builtin_dump_struct(&(x), fprintf, stderr) POP_NO_WARN()

#else
#define dbg(x) ERR_EXPR("dbg macro not supported on this compiler")
#endif

#endif

#define ROUND_UP(value, pow2) (((value) + ((pow2) - 1ull)) & ~((pow2) - 1ull))

#define ISPOW2(value) (popcntl((value)) == 1)
#define ILOG2(value) ((sizeof(unsigned long long) * 8) - 1 - lzcnt((value)))

#if STDC_MIN_23 && __GNUC__
#define UNUSED_ARG(arg) [gnu::unused] arg
#define UNUSED [gnu::unused]
#elif __GNUC__
#define UNUSED_ARG(arg) __attribute__((__unused__)) arg
#define UNUSED __attribute__((__unused__))
#else
#define UNUSED_ARG(arg) arg
#define UNUSED
#endif

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

// fails on one-length arrays, but that is okay because who uses one-length
// arrays ensures it is not a pointer type
#define ARR_LENGTH_RELAXED(a) (sizeof((a)) / sizeof((a)[0]))

// GCC allows ternary but not compound expressions within constant
// clang allows compound but not ternary...
#if __clang__
#define ARR_LENGTH(a)                                                          \
  (ERR_EXPR_COND(sizeof((a)) / sizeof((a)[0]) > 1,                             \
                 "ARR_LENGTH macro used with pointer or one-len array"),       \
   sizeof((a)) / sizeof((a)[0]))
#else
#define ARR_LENGTH(a)                                                          \
  (0 ? ERR_EXPR_COND(sizeof((a)) / sizeof((a)[0]) > 1,                         \
                     "ARR_LENGTH macro used with pointer or one-len array")    \
     : sizeof((a)) / sizeof((a)[0]))
#endif

static inline size_t num_digits(size_t num) {
  return (num ? (size_t)log10((double)num) : 0) + 1;
}

#ifdef SANITIZER_PRINT_STACK_TRACE
static inline void debug_print_stack_trace(void) {
  __sanitizer_print_stack_trace(); // NOLINT(bugprone-signal-handler)
}
#elif !defined(NDEBUG) &&                                                      \
    __has_include(                                                             \
        <execinfo.h>) && __has_include(<unistd.h>) && __has_include(<errno.h>)
#define UTIL_STACK_TRACE_IMPL
void debug_print_stack_trace(void);
#else
static inline void debug_print_stack_trace(void) {}
#endif

#if __clang__

#define DO_PRAGMA(x) _Pragma(#x)
#define PUSH_NO_WARN(warn)                                                     \
  _Pragma("clang diagnostic push") DO_PRAGMA(clang diagnostic ignored warn)
#define POP_NO_WARN() _Pragma("clang diagnostic pop")

#elif __GNUC__ && 0
// this does not work for some reason? errors about "expected declaration
// specifiers before '#pragma'" so disabled

#define DO_PRAGMA(x) _Pragma(#x)
#define PUSH_NO_WARN(warn)                                                     \
  DO_PRAGMA(GCC diagnostic push)                                               \
  DO_PRAGMA(GCC diagnostic ignored warn)

#define POP_NO_WARN() DO_PRAGMA(GCC diagnostic pop)

#else

#define PUSH_NO_WARN(warn)
#define POP_NO_WARN()

#endif

// TEMP: these should go somewhere else
#define PR_RESET "\x1B[0m"
#define PR_RED "\x1B[31m"
#define PR_GREEN "\x1B[32m"
#define PR_YELLOW "\x1B[33m"
#define PR_BLUE "\x1B[34m"
#define PR_MAGENTA "\x1B[35m"
#define PR_CYAN "\x1B[36m"
#define PR_WHITE "\x1B[37m"
#define PR_BOLD "\033[1m"

#define PR_BOLDRED "\x1B[1;31m"
#define PR_BOLDGREEN "\x1B[1;32m"
#define PR_BOLDYELLOW "\x1B[1;33m"
#define PR_BOLDBLUE "\x1B[1;34m"
#define PR_BOLDMAGENTA "\x1B[1;35m"
#define PR_BOLDCYAN "\x1B[1;36m"
#define PR_BOLDWHITE "\x1B[1;37m"

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
    fflush(file);                                                              \
    va_end(v);                                                                 \
  } while (0)

#define MACRO_FMTPRINT(file, message, ...)                                     \
  do {                                                                         \
    fprintf(file, "%s:%d in %s: ", __FILE__, __LINE__, __func__);              \
    fprintf(file, message);                                                    \
    fprintf(file, __VA_ARGS__);                                                \
    fprintf(file, "\n");                                                       \
    fflush(file);                                                              \
  } while (0)

#define EXIT_FAIL(code)                                                        \
  debug_print_stack_trace();                                                   \
  raise(SIGTRAP);                                                              \
  exit(1);

#define TODO_FUNC(sig)                                                         \
  START_NO_UNUSED_ARGS                                                         \
  sig { TODO(__func__); }                                                      \
  END_NO_UNUSED_ARGS

#define TODO(...)                                                              \
  do {                                                                         \
    MACRO_FMTPRINT(stderr, "`TODO` hit, program exiting:  ", __VA_ARGS__);     \
    EXIT_FAIL(-2);                                                             \
  } while (0)

#define BUG(...)                                                               \
  do {                                                                         \
    MACRO_FMTPRINT(stderr, "`BUG` hit, program exiting:  ", __VA_ARGS__);      \
    EXIT_FAIL(-2);                                                             \
  } while (0)

#if NDEBUG
#define DEBUG_ASSERT(b, ...) (void)(b)
#else

// hmm, this does not return the value so can't be used inline to replace an
// expression don't think there is a better way to achieve
//   * DEBUG_ASSERT is valid as an expression
//   * DEBUG_ASSERT is understood by clang analyzer
//   * DEBUG_ASSERT does not double evaluate its arguments

#if HAS_FEATURE(attribute_analyzer_noreturn) && 0
#define DEBUG_ASSERT(b, ...)                                                   \
  (util_debug_assert((b), #b, __func__, __FILE__, __LINE__, __VA_ARGS__),      \
   assert((b)), 0)
// BUG workout why these fail in JCC
// #define DEBUG_ASSERT(b, ...) \
// ((b) ? 0 : (util_debug_assert_fail((b), #b, __func__, __FILE__, __LINE__,
// __VA_ARGS__), 0))
// ((b) ? 0 : ((b) && (util_debug_assert_fail(#b, __func__, __FILE__, __LINE__,
// __VA_ARGS__), 0)))
#else
#define DEBUG_ASSERT(b, ...)                                                   \
  util_debug_assert((b), #b, __func__, __FILE__, __LINE__, __VA_ARGS__)
#endif

PRINTF_ARGS(5)
void util_debug_assert(bool b, const char *cond, const char *func,
                       const char *file, int line, const char *msg, ...);

#endif

#if __STDC_VERSION__ <= 201710L && !defined(unreachable)
NORETURN void unreachable(void);
#endif

PRINTF_ARGS(0) NORETURN void unsupported(const char *msg, ...);

// present in all mode, always causes program exit if fails
// FIXME: clang static analyzer does not recognise this as an assertion function
#define invariant_assert(b, ...)                                               \
  do {                                                                         \
    if (!(b)) {                                                                \
      invariant_assert_fail(__VA_ARGS__);                                      \
    }                                                                          \
  } while (0)

PRINTF_ARGS(0)
NORETURN static inline void invariant_assert_fail(const char *msg, ...) {
  FMTPRINT(stderr, "invariant_assertion failed, program exiting: ", msg);
  EXIT_FAIL(-1)
}

#if HAS_BUILTIN(__builtin_debugtrap)
#define BREAKPOINT() __builtin_debugtrap()
#else
#define BREAKPOINT() raise(SIGINT)
#endif

#define MEMCLR(x) memset((&x), 0, sizeof((x)))

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
void fprint_wstr(FILE *file, const uint32_t *input, size_t len);
size_t sprint_str(char *buf, size_t buf_sz, const char *input, size_t len);

bool try_parse_integer(const char *str, size_t len, unsigned long long *value);

// would like to use `char8_t` for `ustr_t` eventually (but causes lots of
// conflicting type errors atm)

// #if STDC_MIN_23
// #include <uchar.h>
// #else typedef
// unsigned char char8_t;
// #endif

typedef struct {
  const char *str;
  size_t len;
} ustr_t;

static inline bool ustr_nullsafe(ustr_t str) {
  return !memchr(str.str, 0, str.len);
}

static inline bool ustr_eq(ustr_t l, ustr_t r) {
  if (l.len != r.len) {
    return false;
  }

  if (!l.len) {
    return true;
  }

  return !memcmp(l.str, r.str, l.len);
}

static inline bool ustr_prefix(ustr_t str, ustr_t prefix) {
  if (str.len < prefix.len) {
    return false;
  }

  return !memcmp(str.str, prefix.str, prefix.len);
}

static inline bool ustr_suffix(ustr_t str, ustr_t suffix) {
  if (str.len < suffix.len) {
    return false;
  }

  return !memcmp(str.str + str.len - suffix.len, suffix.str, suffix.len);
}

static inline ustr_t ustr_strip_prefix(ustr_t str, ustr_t prefix) {
  if (ustr_prefix(str, prefix)) {
    str.len -= prefix.len;
    str.str += prefix.len;
  }

  return str;
}

static inline ustr_t ustr_strip_suffix(ustr_t str, ustr_t suffix) {
  if (ustr_suffix(str, suffix)) {
    str.len -= suffix.len;
  }

  return str;
}

static inline ustr_t ustr_trim(ustr_t str) {
  if (!str.str) {
    return str;
  }

  const char *start = str.str;
  const char *end = str.str + str.len - 1;

  while (start < end && isspace((unsigned char)start[0])) {
    start++;
  }

  while (end > start && isspace((unsigned char)end[0])) {
    end--;
  }

  DEBUG_ASSERT(end - start >= 0, "trim overflow");
  return (ustr_t){.str = start, .len = (size_t)(end - start + 1)};
}

static inline char *ustr_chr(ustr_t str, int ch) {
  return memchr(str.str, ch, str.len);
}

static inline bool ustr_split(ustr_t str, char delim, ustr_t *l, ustr_t *r) {
  char *pos = ustr_chr(str, delim);

  if (!pos) {
    return false;
  }

  size_t len = (size_t)(pos - str.str);

  *l = (ustr_t){.str = str.str, .len = len};
  *r = (ustr_t){.str = pos + 1, .len = str.len - len - 1};
  return true;
}

static inline int ustr_cmp(ustr_t l, ustr_t r) {
  size_t len = (l.len < r.len) ? l.len : r.len;
  int cmp = memcmp(l.str, r.str, len);

  if (cmp == 0) {
    return (l.len > r.len) - (l.len < r.len);
  }

  return cmp;
}

#if __GNUC__ && !__clang__
// gcc warns on `false ? strlen(NULL) : <other>`
// (which is silly)
static inline size_t wrap_strlen(const char *p) {
  DEBUG_ASSERT(p, "p null");
  return strlen(p);
}
#define MK_USTR(s) ((ustr_t){(s), ((s) != NULL) ? wrap_strlen((s)) : 0})
#else
#define MK_USTR(s) ((ustr_t){(s), ((s) != NULL) ? strlen((s)) : 0})
#endif

#define MK_USTR_LITERAL(s) ((ustr_t){(s), ARR_LENGTH_RELAXED((s))})

// preproc uses null vs empty as a distiction (search 'marker' in file)
// probably want to change that and have these be considered equiv
#define MK_NULL_USTR() ((ustr_t){NULL, 0})
#define MK_EMPTY_USTR() ((ustr_t){"", 0})

#define USTR_SPEC(s) (int)(s).len, (s).str

#endif
