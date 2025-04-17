#ifndef FCACHE_H
#define FCACHE_H

#include "alloc.h"

// fcache is the type used for managing and caching files used by various parts
// of the compiler
struct fcache;

enum FLAG_ENUM fcache_flags {
  FCACHE_FLAG_NONE = 0,

  // assume files on-disk don't change
  // this is used in compilation (rather than LSP mode) for better efficiency
  FCACHE_FLAG_ASSUME_CONSTANT = 1 << 0
};

struct fcache_file {
  // size_t id;
  ustr_t name;

  const char *data;
  size_t len;
};

void fcache_create(struct arena_allocator *arena, enum fcache_flags flags,
                   struct fcache **fcache);

bool fcache_read_stdin(struct fcache *fcache, struct fcache_file *file);

bool fcache_test_path(struct fcache *fcache, ustr_t path);

bool fcache_read_path(struct fcache *fcache, ustr_t path,
                      struct fcache_file *file);
bool fcache_read_proc(struct fcache *fcache, ustr_t proc,
                      struct fcache_file *file);

#endif
