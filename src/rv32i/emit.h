#ifndef RV32I_EMIT_H
#define RV32I_EMIT_H

#include "../emit.h"
#include "../ir/ir.h"

// intervals MUST be sorted such that `interval[i].op_id == i` ID
struct emitted_unit rv32i_emit(const struct cg_unit *unit);

#endif
