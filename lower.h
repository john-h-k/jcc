#ifndef LOWER_H
#define LOWER_H

#include "ir/ir.h"
#include "target.h"

#define IR_REG_IDX_AX (6)
#define IR_REG_IDX_CX (3)
#define IR_REG_IDX_DX (2)
#define IR_REG_IDX_SI (1)
#define IR_REG_IDX_DI (0)

// Performs platform agnostic lowering
void lower(struct ir_unit *unit, const struct target *target);

// needed so that target lowering which generates instructions can lower them
void lower_call(struct ir_func *func, struct ir_op *op);
void lower_bitfield_insert(struct ir_func *func, struct ir_op *op);
void lower_bitfield_extract(struct ir_func *func, struct ir_op *op);
void lower_store(struct ir_func *func, struct ir_op *op);

#endif
