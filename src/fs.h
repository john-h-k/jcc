#ifndef FS_H
#define FS_H

#include "alloc.h"

// fs is the type used for managing and caching files used by various parts
// of the compiler
struct fs;

enum FLAG_ENUM fs_flags {
  FS_FLAG_NONE = 0,

  // assume files on-disk don't change
  // this is used in compilation (rather than LSP mode) for better efficiency
  FS_FLAG_ASSUME_CONSTANT = 1 << 0
};

struct fs_file {
  // size_t id;
  ustr_t name;

  const char *data;
  size_t len;
};

void fs_create(struct arena_allocator *arena, enum fs_flags flags,
                   struct fs **fs);

FILE *fs_tmpfile(const char **path);

bool fs_read_stdin(struct fs *fs, struct fs_file *file);

bool fs_test_path(struct fs *fs, ustr_t path);

bool fs_read_path(struct fs *fs, ustr_t path,
                      struct fs_file *file);
bool fs_read_proc(struct fs *fs, ustr_t proc,
                      struct fs_file *file);

#endif
