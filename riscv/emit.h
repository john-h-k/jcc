#ifndef RISCV_EMIT_H
#define RISCV_EMIT_H

#include "../emit.h"
#include "../ir/build.h"
#include "../lsra.h"

// intervals MUST be sorted such that `interval[i].op_id == i` ID
struct emitted_unit riscv_emit(const struct codegen_unit *unit);

#endif
