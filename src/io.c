#include "io.h"

#include "alloc.h"
#include "util.h"

#include <stdio.h>

struct path_components path_components(struct arena_allocator *arena,
                                       const char *path) {
  const char *last_slash = strrchr(path, '/');

  const char *dir;
  const char *file;
  if (last_slash == NULL) {
    dir = ".";
    file = path;
  } else {
    size_t dir_len = (size_t)(last_slash - path);
    file = last_slash + 1;

    dir = arena_alloc_strndup(arena, path, dir_len);
  }

  char *ext = strrchr(file, '.');
  if (ext) {
    ext++;
  } else {
    ext = "";
  }

  return (struct path_components){.dir = dir, .file = file, .ext = ext};
}

char *path_combine(struct arena_allocator *arena, const char *l,
                   const char *r) {
  size_t l_len = strlen(l);
  size_t r_len = strlen(r);

  size_t total = (l_len ? l_len + 1 : 0) + r_len + 1;
  char *path = arena_alloc(arena, sizeof(*path) * total);

  char *head = path;
  if (l_len) {
    memcpy(head, l, l_len);
    head += l_len;
    *head++ = '/';

    if (r_len && r[0] == '/') {
      r++;
      r_len--;
    }
  }

  memcpy(head, r, r_len);
  head += r_len;
  *head++ = '\0';

  return path;
}

char *path_replace_ext(struct arena_allocator *arena, const char *path,
                       const char *ext) {
  DEBUG_ASSERT(ext[0] != '.', "ext should not start with the `.`");

  // NOTE: if given no extension, this will append
  // this is useful because it gives the invariant that the returned file is
  // always a valid different file e.g
  //   * `path_replace_ext("foo.c", "o")` -> `foo.o`
  //   * `path_replace_ext("foo", "o")` -> `foo.o`

  const char *dot = strrchr(path, '.');

  size_t prefix_len = dot ? (size_t)(dot - path) : strlen(path);
  size_t ext_len = strlen(ext);

  size_t res_sz = prefix_len + 1 + ext_len + 1;

  char *buff = arena_alloc(arena, sizeof(*buff) * res_sz);
  size_t head = 0;

  strncpy(&buff[head], path, prefix_len);
  head += prefix_len;

  buff[head++] = '.';

  strcpy(&buff[head], ext);
  head += ext_len;

  buff[head++] = '\0';

  DEBUG_ASSERT(head == res_sz, "str copy of wrong size");
  return buff;
}

char *path_add_ext(struct arena_allocator *arena, const char *path,
                   const char *ext) {
  DEBUG_ASSERT(ext[0] != '.', "ext should not start with the `.`");

  size_t path_len = strlen(path);
  size_t ext_len = strlen(ext);

  size_t res_sz = path_len + 1 + ext_len + 1;
  char *buff = arena_alloc(arena, sizeof(*buff) * res_sz);
  size_t head = 0;

  strncpy(&buff[head], path, path_len);
  head += path_len;

  buff[head++] = '.';

  strcpy(&buff[head], ext);
  head += ext_len;

  buff[head++] = '\0';

  DEBUG_ASSERT(head == res_sz, "str copy of wrong size");
  return buff;
}
