#ifndef AARCH64_EMIT_H
#define AARCH64_EMIT_H

#include "../emit.h"
#include "../ir/build.h"
#include "../lsra.h"

const char *aarch64_mangle(struct arena_allocator *arena, const char *name);

// intervals MUST be sorted such that `interval[i].op_id == i` ID
struct compiled_function aarch64_emit_function(const struct codegen_function *func);

#endif
