#ifndef EMIT_H
#define EMIT_H

#include <stddef.h>

struct compiled_function {
  const char *name;
  void *code;
  size_t len_code;
};


#endif
