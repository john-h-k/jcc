#ifndef AARCH64_EMIT_H
#define AARCH64_EMIT_H

#include "../emit.h"
#include "../ir/build.h"
#include "../lsra.h"

// intervals MUST be sorted such that `interval[i].op_id == i` ID
struct compiled_function aarch64_emit_function(struct ir_builder *func);

#endif
