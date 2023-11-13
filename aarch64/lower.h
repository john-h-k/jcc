#ifndef AARCH64_LOWER_H
#define AARCH64_LOWER_H

#include "../ir.h"
#include "emitter.h"

struct lower_result {
  const char *name;
  void *code;
  size_t len_code;
};

struct lower_result lower(struct arena_allocator *arena, struct ir_function *func);

#endif
