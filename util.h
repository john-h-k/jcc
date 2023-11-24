#ifndef UTIL_H
#define UTIL_H

#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdnoreturn.h>
#include <string.h>

#define ROUND_UP(value, pow2) ((value) + ((pow2)-1)) & ~((pow2)-1)
#define UNUSED_ARG(arg) (void)(arg);

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

#define ARR_LENGTH(a) sizeof((a)) / sizeof((a)[0])

static inline unsigned long tzcnt(unsigned long l) {
#if defined(__has_builtin) && __has_builtin(__builtin_clz)
  return __builtin_ctz(l);
#else
  todo("lzcnt not implemented outside of `__builtin_clz`");
#endif
}

static inline unsigned long lzcnt(unsigned long l) {
#if defined(__has_builtin) && __has_builtin(__builtin_clz)
  return __builtin_clz(l);
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

[[noreturn]] static inline void todo(const char *msg, ...) {
  FMTPRINT(stderr, "`todo` hit, program exiting: ", msg);
  raise(SIGINT);
  exit(-2);
}

[[noreturn]] static inline void unreachable(const char *msg, ...) {
  FMTPRINT(stderr, "`unreachable` hit, program exiting: ", msg);
  raise(SIGINT);
  exit(-2);
}

[[noreturn]] static inline void bug(const char *msg, ...) {
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

static inline void breakpoint() {
    raise(SIGINT);
}

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
