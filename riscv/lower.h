#ifndef RISCV_LOWER_H
#define RISCV_LOWER_H

#include "../ir/build.h"

// performs platform specific lowering transformations to the IR
void riscv_lower(struct ir_unit *unit);

void riscv_debug_print_custom_ir_op(FILE *file, const struct ir_builder *func,
                                  const struct ir_op *op);

#endif
