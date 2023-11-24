#ifndef AARCH64_EMIT_H
#define AARCH64_EMIT_H

#include "../ir/build.h"
#include "../lsra.h"

struct compiled_function {
  const char *name;
  void *code;
  size_t len_code;
};

// intervals MUST be sorted such that `interval[i].op_id == i` ID
struct compiled_function emit(struct ir_builder *func);

#endif
