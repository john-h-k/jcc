#ifndef IO_H
#define IO_H

#include "alloc.h"

struct path_components {
  const char *dir, *file;
};

struct path_components path_components(struct arena_allocator *arena, const char *path);
char *path_combine(struct arena_allocator *arena, const char *l, const char *r);
char *path_replace_ext(struct arena_allocator *arena, const char *path, const char *ext);
char *path_add_ext(struct arena_allocator *arena, const char *path, const char *ext);
char *read_file(struct arena_allocator *arena, const char *path);

#endif
