#ifndef __UTIL_H__
#define __UTIL_H__

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define ROUND_UP(value, pow2) ((value) + ((pow2) - 1)) & ~((pow2) - 1)
#define UNUSED_ARG(arg) (void)(arg);

[[ noreturn ]]
static inline void todo(const char *msg) {
  fprintf(stderr, "`todo` hit, program exiting: %s\n", msg);
  exit(-2);
}

[[ noreturn ]]
static inline void unreachable(const char *msg) {
  fprintf(stderr, "`unreachable` hit, program exiting: %s\n", msg);
  exit(-2);
}

// present in all mode, always causes program exit if fails
static inline void invariant_assert(bool b, const char *msg) {
  if (!b) {
    fprintf(stderr, "invariant_assertion failed, program exiting: %s\n", msg);
    exit(-1);
  }
}

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
