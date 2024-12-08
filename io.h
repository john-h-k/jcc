#ifndef IO_H
#define IO_H

char *path_dir(const char *path);
char *path_combine(const char *l, const char *r);
char *path_replace_ext(const char *path, const char *ext);
char *path_add_ext(const char *path, const char *ext);
char *read_file(const char *path);

#endif
