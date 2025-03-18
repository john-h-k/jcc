#ifndef AARCH64_LOWER_H
#define AARCH64_LOWER_H

#include "../ir/ir.h"

// performs platform specific lowering transformations to the IR
void aarch64_lower(struct ir_unit *unit);

struct ir_func_info aarch64_lower_func_ty(struct ir_func *func,
                                         struct ir_var_func_ty func_ty,
                                         struct ir_op **args, size_t num_args);

void aarch64_debug_print_custom_ir_op(FILE *file, const struct ir_func *func,
                                      const struct ir_op *op);

#endif
