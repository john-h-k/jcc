#ifndef X64_LOWER_H
#define X64_LOWER_H

#include <stdio.h>
#include "../ir/ir.h"

// performs platform specific lowering transformations to the IR
void x64_lower_variadic(struct ir_func *func);
void x64_lower(struct ir_unit *unit);

struct ir_func_info x64_lower_func_ty(struct ir_func *func,
                                      struct ir_var_func_ty func_ty,
                                      struct ir_op **args, size_t num_args);

void x64_debug_print_custom_ir_op(FILE *file, const struct ir_func *func,
                                  const struct ir_op *op);

#endif
