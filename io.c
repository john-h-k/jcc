#include "io.h"

#include "alloc.h"
#include "util.h"

#include <stdio.h>

// FIXME: these all leak. make them use `arena`s

struct path_components path_components(struct arena_allocator *arena, const char *path) {
  const char *last_slash = strrchr(path, '/');

  if (last_slash == NULL) {
    return (struct path_components){.dir = ".", .file = arena_alloc_strcpy(arena, path)};
  }

  size_t dir_len = (size_t)(last_slash - path);
  const char *file_part = last_slash + 1;

  char *dir = arena_alloc(arena, dir_len + 1);
  strncpy(dir, path, dir_len);
  dir[dir_len] = '\0';

  return (struct path_components){.dir = dir, .file =  arena_alloc_strcpy(arena, file_part)};
}

char *path_combine(struct arena_allocator *arena, const char *l, const char *r) {
  size_t l_len = strlen(l);
  size_t r_len = strlen(r);

  size_t total = l_len + 1 + r_len + 1;
  char *path = arena_alloc(arena, sizeof(*path) * total);

  char *head = path;
  memcpy(head, l, l_len);
  head += l_len;
  *head++ = '/';
  memcpy(head, r, r_len);
  head += r_len;
  *head++ = '\0';

  return path;
}

char *path_replace_ext(struct arena_allocator *arena, const char *path, const char *ext) {
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

char *path_add_ext(struct arena_allocator *arena, const char *path, const char *ext) {
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

char *read_path(struct arena_allocator *arena, const char *path) {
  FILE *file = fopen(path, "r");

  if (!file) {
    return NULL;
  }

  return read_file(arena, file);
}

char *read_file(struct arena_allocator *arena, FILE *file) {
  fseek(file, 0, SEEK_END);
  long fsize = ftell(file);

  if (fsize == -1L) {
    // can't tell size of file (e.g stdin), just iteratively read instead
    #define READ_BUF_SZ (4096)

    size_t read = 0;
    size_t len = 0;

    char *content = NULL;
    char *head;

    do {
      content = arena_realloc(arena, content, len + READ_BUF_SZ);
      head = &content[len];

      len += read;

    } while ((read = fread(head, 1, READ_BUF_SZ, file)) > 0);

    fclose(file);

    content[len] = '\0';
    return content;
  }

  rewind(file);

  char *content = arena_alloc(arena, (unsigned long)fsize + 1);
  size_t read = fread(content, 1, (unsigned long)fsize, file);
  fclose(file);

  if (read != (size_t)fsize) {
    return NULL;
  }

  content[fsize] = '\0';
  return content;
}
