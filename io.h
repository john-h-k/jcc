#ifndef IO_H
#define IO_H

struct path_components {
  const char *dir, *file;
};

struct path_components path_components(const char *path);
char *path_combine(const char *l, const char *r);
char *path_replace_ext(const char *path, const char *ext);
char *path_add_ext(const char *path, const char *ext);
char *read_file(const char *path);

#endif
