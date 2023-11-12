#ifndef ARM64_LOWER_H
#define ARM64_LOWER_H

#include "ir.h"
#include "arm64_emitter.h"

struct lower_result {
  void* code;
  size_t len_code;
};

struct lower_result lower(struct arena_allocator *arena, struct ir_function *func);

#endif
