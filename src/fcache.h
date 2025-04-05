#ifndef FCACHE_H
#define FCACHE_H

#include "alloc.h"

// fcache is the type used for managing and caching files used by various parts of the compiler
struct fcache;

enum fcache_flags {
  FCACHE_FLAG_NONE = 0,

  // assume files on-disk don't change
  // this is used in compilation (rather than LSP mode) for better efficiency
  FCACHE_FLAG_ASSUME_CONSTANT = 1 << 0
};

struct fcache_file {
  // size_t id;
  struct sized_str name;

  const char *data;
  size_t len;
};

void fcache_create(struct arena_allocator *arena, enum fcache_flags flags, struct fcache **fcache);

bool fcache_read_stdin(struct fcache *fcache, struct fcache_file *file);

bool fcache_test_path(struct fcache *fcache, struct sized_str path);

bool fcache_read_path(struct fcache *fcache, struct sized_str path, struct fcache_file *file);
bool fcache_read_proc(struct fcache *fcache, struct sized_str proc, struct fcache_file *file);

#endif
