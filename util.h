#ifndef UTIL_H
#define UTIL_H

#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <string.h>

#define ROUND_UP(value, pow2) ((value) + ((pow2)-1)) & ~((pow2)-1)
#define UNUSED_ARG(arg) (void)(arg);

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

static inline unsigned long tzcnt(unsigned long l) {
#if defined(__has_builtin) && __has_builtin (__builtin_clz)
  return __builtin_ctz(l);
#else
  todo("lzcnt not implemented outside of `__builtin_clz`");
#endif
}


[[noreturn]] static inline void todo(const char *msg) {
  fprintf(stderr, "`todo` hit, program exiting: %s\n", msg);
  raise(SIGINT);
  exit(-2);
}

[[noreturn]] static inline void unreachable(const char *msg) {
  fprintf(stderr, "`unreachable` hit, program exiting: %s\n", msg);
  raise(SIGINT);
  exit(-2);
}

[[noreturn]] static inline void bug(const char *msg) {
  fprintf(stderr, "`bug` hit, program exiting: %s\n", msg);
  raise(SIGINT);
  exit(-2);
}

// present in all mode, always causes program exit if fails
static inline void invariant_assert(bool b, const char *msg) {
  if (!b) {
    fprintf(stderr, "invariant_assertion failed, program exiting: %s\n", msg);
    raise(SIGINT);
    exit(-1);
  }
}

#if NDEBUG
static inline void debug_assert(bool, const char *) {}
#else
static inline void debug_assert(bool b, const char *msg) {
  if (!b) {
    fprintf(stderr, "debug_assertion failed, program exiting: %s\n", msg);
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
