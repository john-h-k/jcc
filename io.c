#include "io.h"
#include "util.h"
#include <stdio.h>

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
