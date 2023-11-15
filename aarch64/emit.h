#ifndef AARCH64_EMIT_H
#define AARCH64_EMIT_H

#include "../ir.h"

struct compiled_function {
  const char *name;
  void *code;
  size_t len_code;
};

struct compiled_function emit(struct ir_builder *func);

#endif
