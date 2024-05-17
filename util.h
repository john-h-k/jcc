#ifndef UTIL_H
#define UTIL_H

// TODO: seperate bool/noreturn and other version-dependent stuff into its own header

#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef __STDC_VERSION__ 
#error "JCC Requires at least C99"
#endif

#define NORETURN

#if __STDC_VERSION__ >= 201112L
#undef NORETURN
#include <stdnoreturn.h>

#if __STDC_VERSION__ >= 202311L
#define NORETURN [[noreturn]]
#else
#define NORETURN noreturn
#endif

#endif

#include <string.h>

#define ROUND_UP(value, pow2) ((value) + ((pow2)-1)) & ~((pow2)-1)
#define UNUSED_ARG(arg) (void)(arg);

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

#define ARR_LENGTH(a) (sizeof((a)) / sizeof((a)[0]))

static inline unsigned long popcntl(unsigned long l) {
#if defined(__has_builtin) && __has_builtin(__builtin_popcountl)
  return __builtin_popcountl(l);
#else
  todo("lzcnt not implemented outside of `__builtin_popcountl`");
#endif
}

static inline unsigned long tzcnt(unsigned long l) {
#if defined(__has_builtin) && __has_builtin(__builtin_clz)
  return __builtin_ctzl(l);
#else
  todo("lzcnt not implemented outside of `__builtin_clz`");
#endif
}

static inline unsigned long lzcnt(unsigned long l) {
#if defined(__has_builtin) && __has_builtin(__builtin_clz)
  return __builtin_clzl(l);
#else
  todo("lzcnt not implemented outside of `__builtin_clz`");
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

NORETURN static inline void todo(const char *msg, ...) {
  FMTPRINT(stderr, "`todo` hit, program exiting: ", msg);
  raise(SIGINT);
  exit(-2);
}

NORETURN static inline void unreachable(const char *msg, ...) {
  FMTPRINT(stderr, "`unreachable` hit, program exiting: ", msg);
  raise(SIGINT);
  exit(-2);
}

NORETURN static inline void bug(const char *msg, ...) {
  FMTPRINT(stderr, "`bug` hit, program exiting: ", msg);
  raise(SIGINT);
  exit(-2);
}

// present in all mode, always causes program exit if fails
static inline void invariant_assert(bool b, const char *msg, ...) {
  if (!b) {
    FMTPRINT(stderr, "invariant_assertion failed, program exiting: ", msg);
    raise(SIGINT);
    exit(-1);
  }
}

static inline void breakpoint(void) { raise(SIGINT); }

#if NDEBUG
static inline void debug_assert(bool, const char *, ...) {}
#else
static inline void debug_assert(bool b, const char *msg, ...) {
  if (!b) {
    FMTPRINT(stderr, "debug_assertion failed, program exiting: ", msg);
    raise(SIGINT);
    exit(-1);
  }
}
#endif

static inline void *nonnull_malloc(size_t size) {
  void *ptr = malloc(size);

  invariant_assert(ptr, "`malloc` returned NULL (out-of-memory likely)");

  return ptr;
}

static inline char *malloc_strcpy(const char *s) {
  int len = strlen(s);

  char *cp = nonnull_malloc(len * sizeof(*s));

  // use memcpy as we already have len
  memcpy(cp, s, len * sizeof(*s));

  return cp;
}

#endif
