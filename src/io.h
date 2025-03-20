#ifndef IO_H
#define IO_H

#include "alloc.h"

struct path_components {
  // `foo/bar.c` -> `dir=foo, file=bar.c, ext=c`
  const char *dir, *file, *ext;
};

struct path_components path_components(struct arena_allocator *arena,
                                       const char *path);
char *path_combine(struct arena_allocator *arena, const char *l, const char *r);
char *path_replace_ext(struct arena_allocator *arena, const char *path,
                       const char *ext);
char *path_add_ext(struct arena_allocator *arena, const char *path,
                   const char *ext);
char *read_path(struct arena_allocator *arena, const char *path);
char *read_file(struct arena_allocator *arena, FILE *file);

#endif
