#include "io.h"
#include "util.h"
#include <stdio.h>

// FIXME: these all leak. make them use `arena`s

char *path_combine(const char *l, const char *r) {
  size_t l_len = strlen(l);
  size_t r_len = strlen(r);

  size_t total = l_len + 1 + r_len + 1;
  char *path = nonnull_malloc(sizeof(*path) * total);

  char *head = path;
  memcpy(head, l, l_len);
  head += l_len;
  *head++ = '/';
  memcpy(head, r, r_len);
  head += r_len;
  *head++ = '\0';

  return path;
}

char *path_replace_ext(const char *path, const char *ext) {
  debug_assert(ext[0] != '.', "ext should not start with the `.`");

  // NOTE: if given no extension, this will append
  // this is useful because it gives the invariant that the returned file is always a valid different file
  // e.g
  //   * `path_replace_ext("foo.c", "o")` -> `foo.o`
  //   * `path_replace_ext("foo", "o")` -> `foo.o`

  const char *dot = strrchr(path, '.');

  size_t prefix_len = dot ? dot - path : strlen(path);
  size_t ext_len = strlen(ext);

  size_t res_sz = prefix_len + 1 + ext_len + 1;

  char *buff = nonnull_malloc(sizeof(char) * res_sz);
  size_t head = 0;

  strncpy(&buff[head], path, prefix_len);
  head += prefix_len;

  buff[head++] = '.';

  strcpy(&buff[head], ext);
  head += ext_len;

  buff[head++] = '\0';

  debug_assert(head == res_sz, "str copy of wrong size");
  return buff;
}

char *path_add_ext(const char *path, const char *ext) {
  debug_assert(ext[0] != '.', "ext should not start with the `.`");

  size_t path_len = strlen(path);
  size_t ext_len = strlen(ext);

  size_t res_sz = path_len + 1 + ext_len + 1;
  char *buff = nonnull_malloc(sizeof(char) * res_sz);
  size_t head = 0;

  strncpy(&buff[head], path, path_len);
  head += path_len;

  buff[head++] = '.';

  strcpy(&buff[head], ext);
  head += ext_len;

  buff[head++] = '\0';

  debug_assert(head == res_sz, "str copy of wrong size");
  return buff;
}

char *read_file(const char *path) {
  FILE *f = fopen(path, "r");

  if (!f) {
    return NULL;
  }

  fseek(f, 0, SEEK_END);
  long fsize = ftell(f);

  invariant_assert(fsize != -1L, "ftell failed");

  rewind(f);

  char *content = nonnull_malloc((unsigned long)fsize + 1);
  fread(content, (unsigned long)fsize, 1, f);
  fclose(f);

  content[fsize] = '\0';
  return content;
}
